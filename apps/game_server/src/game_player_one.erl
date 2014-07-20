-module(game_player_one).
-behavior(gen_server).

-compile([{parse_transform, lager_transform}]).

-include("user.hrl").
-include("round.hrl").
-include("table.hrl").
-include("game.hrl").
-include("dealer.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_link/6,bet/3,user_disconnect/2]).
-record(state,{game,player_table,user,bet_ets,server,user_pid}).

-define(JST(Kind,Table,Content),[{<<"kind">>,Kind},{<<"content">>,[{<<"table">>,Table}|Content]}]).

-define(CASINO_DB,mysql_casino_master).

bet(GameServer,Cats,Amounts)->
	gen_server:call(GameServer,{bet,Cats,Amounts}).

user_disconnect(GameServer,UserPid)->
	gen_server:cast(GameServer,{user_disconnect,UserPid}).

start_link(Game,DealerTable,Server,PlayerTable,User,UserPid)->
	gen_server:start_link(?MODULE,{Game,DealerTable,Server,PlayerTable,User,UserPid},[]).

init({Game,DealerTable,Server,PlayerTable,User,UserPid})->
	gproc:reg({n,l,{PlayerTable#player_table.id,User#user.id}}),
	casino_events:subscribe(DealerTable),
	erlang:monitor(process,UserPid),
	BetEts=ets:new(player_bets,[set,private]),
	{ok,#state{game=Game,player_table=PlayerTable,user=User,server=Server,bet_ets=BetEts,user_pid=UserPid}}.

do_bet(Server,BetEts,UserId,PlayerTableId,Cats,Amounts)->
	case game_api:try_bet(Server,Cats,Amounts) of
		{ok,RoundId}->
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
					
handle_call(Event={bet,Cats,Amounts},_From,State=#state{server=Server,user=User,player_table=PlayerTable,bet_ets=BetEts})->
	lager:info("bet module ~p, event ~p, state ~p",[?MODULE,Event,State]),
	Result=do_bet(Server,BetEts,User#user.id,PlayerTable#player_table.id,Cats,Amounts),
	{reply,Result,State}.

handle_cast({user_disconnect,UserPid},State=#state{user_pid=UserPid})->
	{noreply,State#state{user_pid=undefined}};

handle_cast(Request,State)->
	lager:error("unexpected Request ~p, State ~p",[Request,State]),
	{noreply,State}.

handle_info(Info={'DOWN',_Ref,process,UserPid,_},State=#state{user_pid=UserPid})->
	lager:error("handle player process DOWN, info ~p,state ~p",[Info,State]),
	{noreply,State#state{user_pid=undefined}};

handle_info({user_reconnect,UserPid},State=#state{user_pid=undefined})->
	{noreply,State#state{user_pid=UserPid}};

handle_info({start_bet,_}=Info,State=#state{bet_ets=BetEts,user_pid=UserPid})->
	ets:delete_all_objects(BetEts),
	send_json(UserPid,json(Info)),
	{noreply,State};

handle_info({commit,{Table,Round,Cards,Cstr}},State=#state{game=Game,bet_ets=BetEts,user=User,user_pid=UserPid,player_table=PlayerTable})->
	RatioMap=(Game#game.module):payout(Cards,PlayerTable#player_table.payout),
	{Pb,Pt}=casino_bets:player_payout(BetEts,RatioMap),
	casino_bets:persist_payout(Round#round.id,User#user.id,PlayerTable#player_table.id,Pb,Pt),
	send_json(UserPid,json({commit,{Table,Cstr,Round#round.id,Pt}})),
	{noreply,State};

handle_info(Info,State=#state{user_pid=UserPid})->
	lager:error("module ~p, Info ~p, State ~p",[?MODULE,Info,State]),
	send_json(UserPid,json(Info)),
	{noreply,State}.

terminate(Reason,State=#state{bet_ets=BetEts})->
	lager:info("terminate, Reason ~p, State ~p",[Reason,State]),
	ets:delete(BetEts),
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.

send_json(UserPid,Json)->
	case {UserPid,Json} of
		{undefined,_}->
			ok;
		{_,error}->
			ok;
		_ ->
			UserPid ! {json,jsx:encode(Json)}
	end.

ensure_binary(Input) when is_list(Input)->
	list_to_binary(Input);
ensure_binary(Input) when is_binary(Input)->
	Input.

json({deal,{Table,Pos,CardL}})->
	?JST(deal,Table,[{<<"pos">>,Pos},{<<"card">>,CardL}]);

json({clear,{Table,Pos}})->
	?JST(clear,Table,[{<<"pos">>,Pos}]);

json({dealer_disconnect,{Table,#dealer{id=DealerId,name=DealerName}}})->
	?JST(dealer_disconnect,Table,[{<<"id">>,DealerId},{<<"name">>,ensure_binary(DealerName)}]);

json({dealer_connect,{Table,#dealer{id=DealerId,name=DealerName}}})->
	?JST(dealer_connect,Table,[{<<"id">>,DealerId},{<<"name">>,ensure_binary(DealerName)}]);		

json({tick,{Table,Value}})->
	?JST(tick,Table,[{<<"countdown">>,Value}]);

json({commit,{Table,Cards,RoundId,Payout}})->
	?JST(commit,Table,[{<<"round_id">>,RoundId},{<<"cards">>,Cards},{<<"payout">>,Payout}]);

json({start_bet,{Table,#round{id=RoundId,shoeIndex=ShoeIndex,roundIndex=RoundIndex},Countdown}})->
	Content=[{<<"round_id">>,RoundId},{<<"shoe_index">>,ShoeIndex},{<<"round_index">>,RoundIndex},{<<"countdown">>,Countdown}],
	?JST(start_bet,Table,Content);

json(_Info)->
	error.