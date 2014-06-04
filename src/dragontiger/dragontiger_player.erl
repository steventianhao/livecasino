-module(dragontiger_player).
-behavior(gen_server).
-include("user.hrl").
-include("round.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([bet/3,start_link/4]).
-record(state,{player_table_id,user,bet_ets,server,eventbus,round_id}).

-define(GAME_PLAYER_MOD,dragontiger_player_mod).
-define(CASINO_DB,mysql_casino_master).

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

do_bet(Server,BetEts,RoundId,UserId,PlayerTableId,Cats,Amounts)->
	case dragontiger_game_api:try_bet(Server,Cats,Amounts) of
		ok->
			Bet=casino_bets:create_bet_req(RoundId,UserId,PlayerTableId,Cats,Amounts),
			case mysql_db:user_bet(?CASINO_DB,Bet) of
				{ok,Bundle={BetBundleId,_BalanceAfter}}->
					true=casino_bets:insert_bets(BetEts,BetBundleId,Cats,Amounts),
					{ok,Bundle};
				Error->
					Error
			end;
		Res ->
			Res
	end.
					

handle_call(Event={bet,Cats,Amounts},_From,State=#state{server=Server,user=User,player_table_id=PlayerTableId,bet_ets=BetEts,round_id=RoundId})->
	lager:info("bet module ~p, event ~p, state ~p",[?MODULE,Event,State]),
	Result=case RoundId of
		undefined->
			{error,round_not_found};
		_ ->
			do_bet(Server,BetEts,RoundId,User#user.id,PlayerTableId,Cats,Amounts)
	end,
	{reply,Result,State}.	
	

handle_cast(Request,State)->
	lager:error("unexpected Request ~p, State ~p",[Request,State]),
	{noreply,State}.

handle_info({json,Json},State)->
	lager:info("json ~p, state ~p",[Json,State]),
	{noreply,State};

handle_info({start_bet,{_Table,Round,_Countdown}},State=#state{bet_ets=BetEts})->
	#round{id=RoundId}=Round,
	ets:delete_all_objects(BetEts),
	lager:info("start_bet, round is ~p",[Round]),
	{noreply,State#state{round_id=RoundId}};

handle_info({commit,{_Table,Cards}},State=#state{bet_ets=BetEts,round_id=RoundId,user=User,player_table_id=PlayerTableId})->
	%%atom dragontiger should be the payout scheme, when create this process, should be passed in.
	RatioMap=?GAME_PLAYER_MOD:payout(Cards,dragontiger),
	casino_bets:payout_bets(BetEts,RatioMap),
	Pb=casino_bets:payout_bundles(BetEts),
	Pt=casino_bets:payout_total(Pb),
	Payout=casino_bets:create_payout_req(RoundId,User#user.id,PlayerTableId,Pb,Pt),
	mysql_db:user_payout(?CASINO_DB,Payout),
	lager:info("payout by bundles ~p, payout total ~p",[Pb,Pt]),
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