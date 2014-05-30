-module(dragontiger_player).
-behavior(gen_server).
-include("user.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([bet/3,start_link/4]).
-record(state,{player_table_id,user,bet_ets,server,eventbus}).

-define(GAME_PLAYER_MOD,dragontiger_player_mod).
-define(CASINO_DB,mysql_casino_master).

insert_bets(BetEts,BetBundleId,Cats,Amounts)->
	Ts=lists:zipwith(fun(C,A)->{{BetBundleId,C},A,0} end, Cats, Amounts),
	ets:insert(BetEts,Ts).

payout_bet('$end_of_table',_BetEts,_RatioMap)->
	ok;
payout_bet(Key={_,Cat},BetEts,RatioMap)->
	Ratio=maps:get(Cat,RatioMap),
	ets:update_element(BetEts,Key,{3,Ratio}),
	payout_bet(ets:next(BetEts,Key),BetEts,RatioMap).
payout_bets(BetEts,RatioMap)->
	payout_bet(ets:first(BetEts),BetEts,RatioMap).

start_link(Server,EventBus,PlayerTableId,User) when is_pid(Server) andalso is_pid(EventBus) andalso is_integer(PlayerTableId) andalso is_record(User,user)->
	gen_server:start_link(?MODULE,{Server,EventBus,PlayerTableId,User},[]).

bet(Pid,Cats,Amounts)->
	case ?GAME_PLAYER_MOD:is_valid_bets(Cats,Amounts) of
		true ->
			gen_server:call(Pid,{bet,Cats,Amounts});
		_ ->
			{error,invalid_bets}
	end.

init({Server,EventBus,PlayerTableId,User=#user{id=UserId}})->
	dragontiger_player_handler:add_handler(EventBus,UserId),
	BetEts=ets:new(player_bets,[set]),
	{ok,#state{player_table_id=PlayerTableId,user=User,server=Server,eventbus=EventBus,bet_ets=BetEts}}.

handle_call(Event={bet,Cats,Amounts},_From,State=#state{server=Server,user=User,player_table_id=PlayerTableId,bet_ets=BetEts})->
	lager:info("bet module ~p, event ~p, state ~p",[?MODULE,Event,State]),
	case dragontiger_game_api:try_bet(Server,Cats,Amounts) of
		{ok,Tag,RoundId}->
			lager:info("after try_bet, tag ~p, roundId ~p",[Tag,RoundId]),
			Bet=casino_bets:create_bet_req(RoundId,User#user.id,PlayerTableId,Cats,Amounts),
			case mysql_db:user_bet(?CASINO_DB,Bet) of
				{ok,Bundle={BetBundleId,_BalanceAfter}}->
					insert_bets(BetEts,BetBundleId,Cats,Amounts),
					{reply,{ok,Bundle},State};
				Error->
					{reply,Error,State}
			end;
		Res ->
			lager:info("after try_bet, res ~p",[Res]),
			{reply,error,State}
	end.

handle_cast(Request,State)->
	lager:error("unexpected Request ~p, State ~p",[Request,State]),
	{noreply,State}.

handle_info({json,Json},State)->
	lager:info("json ~p, state ~p",[Json,State]),
	{noreply,State};

handle_info({start_bet,_},State=#state{bet_ets=BetEts})->
	ets:delete_all_objects(BetEts),
	{noreply,State};

handle_info({commit,{_Table,Cards}},State=#state{bet_ets=BetEts})->
	%%atom dragontiger should be the payout scheme, when create this process, should be passed in.
	RatioMap=?GAME_PLAYER_MOD:payout(Cards,dragontiger),
	payout_bets(BetEts,RatioMap),
	%%foldl to caculate the payout for each bundle send to db to update 
	%%foldl to caculate all, send to player for display.
	{noreply,State};



handle_info(Info,State)->
	lager:error("module ~p, Info ~p, State ~p",[?MODULE,Info,State]),
	{noreply,State}.

terminate(Reason,State=#state{user=#user{id=UserId},eventbus=EventBus})->
	lager:info("terminate, Reason ~p, State ~p",[Reason,State]),
	dragontiger_player_handler:del_handler(EventBus,UserId,Reason),
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.