-module(dragontiger_player).
-behavior(gen_server).
-include("user.hrl").
-include("db.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([bet/3,start_link/4]).
-record(state,{player_table_id,user,server,eventbus}).

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
	{ok,#state{player_table_id=PlayerTableId,user=User,server=Server,eventbus=EventBus}}.

handle_call(_Event={bet,Cats,Amounts},_From,State=#state{server=Server,user=User,player_table_id=PlayerTableId})->
	case dragontiger_game_api:try_bet(Server,Cats,Amounts) of
		{ok,Tag,RoundId}->
			Bet=casino_bets:create_bet_req(RoundId,User#user.id,PlayerTableId,Cats,Amounts),
			mysql_db:user_bet(?CASINO_DB,Bet),
			{reply,ok,State};
		_ ->
			{reply,error,State}
	end.

	%% first, test to save the bets into db
	%%			{ok,#db_bet_res{bet_bundle_id=BetBundleId}}->
	%%				dragontiger_game_api:bet_succeed(Server,Tag,BetBundleId),
	%%				ok;
	%%			Error={error,insufficient_balance} ->
	%%				dragontiger_game_api:bet_failed(Server,Tag),
	%%				Error
	%%		end.
	%%	Err={error,_}->
	%%		Err
	%%end.
	

handle_cast(Request,State)->
	lager:error("unexpected Request ~p, State ~p",[Request,State]),
	{noreply,State}.

handle_info({json,Json},State)->
	lager:info("json ~p, state ~p",[Json,State]),
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