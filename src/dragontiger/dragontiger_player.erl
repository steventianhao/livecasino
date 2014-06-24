-module(dragontiger_player).
-behavior(gen_server).
-include("user.hrl").
-include("round.hrl").
-include("table.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([bet/3,start_link/4]).
-record(state,{player_table,user,bet_ets,server,round_id}).

-define(CASINO_DB,mysql_casino_master).
-define(GAME_PLAYER_MOD,dragontiger_player_mod).
-define(PLAYER_HANDLER_MOD,dragontiger_player_handler).
-define(GAME_API,dragontiger_game_api).

bet(Pid,Cats,Amounts)->
	case ?GAME_PLAYER_MOD:is_valid_bets(Cats,Amounts) of
		true ->
			gen_server:call(Pid,{bet,Cats,Amounts});
		_ ->
			{error,invalid_bets}
	end.

start_link(Server,EventBus,PlayerTable,User)->
	gen_server:start_link(?MODULE,{Server,EventBus,PlayerTable,User},[]).

init({Server,EventBus,PlayerTable,User})->
	gen_event:add_handler(EventBus,{?PLAYER_HANDLER_MOD,User#user.id},self()),
	BetEts=ets:new(player_bets,[set,private]),
	{ok,#state{player_table=PlayerTable,user=User,server=Server,bet_ets=BetEts}}.

do_bet(Server,BetEts,RoundId,UserId,PlayerTableId,Cats,Amounts)->
	case ?GAME_API:try_bet(Server,Cats,Amounts) of
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
					
handle_call(Event={bet,Cats,Amounts},_From,State=#state{server=Server,user=User,player_table=PlayerTable,bet_ets=BetEts,round_id=RoundId})->
	lager:info("bet module ~p, event ~p, state ~p",[?MODULE,Event,State]),
	Result=case RoundId of
		undefined->
			{error,round_not_found};
		_ ->
			do_bet(Server,BetEts,RoundId,User#user.id,PlayerTable#player_table.id,Cats,Amounts)
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

handle_info({commit,{_Table,Cards}},State=#state{bet_ets=BetEts,round_id=RoundId,user=User,player_table=#player_table{id=PlayerTableId,payout=PayoutSchema}})->
	RatioMap=?GAME_PLAYER_MOD:payout(Cards,PayoutSchema),
	{Pb,Pt}=casino_bets:player_payout(BetEts,RatioMap),
	casino_bets:persist_payout(RoundId,User#user.id,PlayerTableId,Pb,Pt),
	lager:info("payout by bundles ~p, payout total ~p",[Pb,Pt]),
	{noreply,State};

handle_info(Info,State)->
	lager:error("module ~p, Info ~p, State ~p",[?MODULE,Info,State]),
	{noreply,State}.

terminate(Reason,State=#state{user=User,server=Server,bet_ets=BetEts})->
	lager:info("terminate, Reason ~p, State ~p",[Reason,State]),
	?GAME_API:player_quit(Server,User,Reason),
	ets:delete(BetEts),
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.