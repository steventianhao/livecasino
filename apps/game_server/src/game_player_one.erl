-module(game_player_one).
-behavior(gen_server).

-compile([{parse_transform, lager_transform}]).

-include("user.hrl").
-include("round.hrl").
-include("table.hrl").
-include("game.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_link/6,bet/3]).
-record(state,{game,player_table,user,bet_ets,server,round_id,user_pid}).

-define(CASINO_DB,mysql_casino_master).

bet(GameServer,Cats,Amounts)->
	gen_server:call(GameServer,{bet,Cats,Amounts}).

start_link(Game,DealerTable,Server,PlayerTable,User,UserPid)->
	gen_server:start_link(?MODULE,{Game,DealerTable,Server,PlayerTable,User,UserPid},[]).

init({Game,DealerTable,Server,PlayerTable,User,UserPid})->
	gproc:reg({n,l,{PlayerTable#player_table.id,User#user.id}}),
	casino_events:subscribe(DealerTable),
	BetEts=ets:new(player_bets,[set,private]),
	{ok,#state{game=Game,player_table=PlayerTable,user=User,server=Server,bet_ets=BetEts,user_pid=UserPid}}.

do_bet(Module,Server,BetEts,RoundId,UserId,PlayerTableId,Cats,Amounts)->
	case Module:try_bet(Server,Cats,Amounts) of
		ok->
			case casino_bets:persist_bet(RoundId,UserId,PlayerTableId,Cats,Amounts) of
				{ok,Bundle={BetBundleId,_BalanceAfter}}->
					true=casino_bets:insert_bets(BetEts,BetBundleId,Cats,Amounts),
					{ok,Bundle};
				Error->
					Error
			end;
		Res ->
			Res
	end.
					
handle_call(Event={bet,Cats,Amounts},_From,State=#state{game=Game,server=Server,user=User,player_table=PlayerTable,bet_ets=BetEts,round_id=RoundId})->
	lager:info("bet module ~p, event ~p, state ~p",[?MODULE,Event,State]),
	Result=case RoundId of
		undefined->
			{error,round_not_found};
		_ ->
			do_bet(Game#game.module,Server,BetEts,RoundId,User#user.id,PlayerTable#player_table.id,Cats,Amounts)
	end,
	{reply,Result,State}.

handle_cast(Request,State)->
	lager:error("unexpected Request ~p, State ~p",[Request,State]),
	{noreply,State}.

handle_info({start_bet,{Table,Round,Countdown}},State=#state{bet_ets=BetEts,user_pid=UserPid})->
	ets:delete_all_objects(BetEts),
	send_json(UserPid,json(start_bet,{Table,Round,Countdown})),
	{noreply,State#state{round_id=Round#round.id}};

handle_info({commit,{Table,Cards,Cstr}},State=#state{game=Game,bet_ets=BetEts,round_id=RoundId,user=User,user_pid=UserPid,player_table=PlayerTable})->
	RatioMap=(Game#game.module):payout(Cards,PlayerTable#player_table.payout),
	{Pb,Pt}=casino_bets:player_payout(BetEts,RatioMap),
	casino_bets:persist_payout(RoundId,User#user.id,PlayerTable#player_table.id,Pb,Pt),
	send_json(UserPid,json(commit,{Table,Cstr,RoundId,Pt})),
	{noreply,State};

handle_info(Info,State)->
	lager:error("module ~p, Info ~p, State ~p",[?MODULE,Info,State]),
	{noreply,State}.

terminate(Reason,State=#state{bet_ets=BetEts})->
	lager:info("terminate, Reason ~p, State ~p",[Reason,State]),
	ets:delete(BetEts),
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.

send_json(UserPid,Json)->
	UserPid ! {json,jsx:encode(Json)}.

json(commit,{Table,Cards,RoundId,Payout})->
	[{<<"kind">>,commit},{<<"content">>,
		[{<<"table">>,Table},{<<"round_id">>,RoundId},{<<"cards">>,Cards},{<<"payout">>,Payout}]}];

json(start_bet,{Table,Round,Countdown})->
	#round{id=RoundId,shoeIndex=ShoeIndex,roundIndex=RoundIndex}=Round,
	[{<<"kind">>,start_bet},{<<"content">>,
		[{<<"table">>,Table},{<<"round_id">>,RoundId},{<<"shoe_index">>,ShoeIndex},
		{<<"round_index">>,RoundIndex},{<<"countdown">>,Countdown}]}].