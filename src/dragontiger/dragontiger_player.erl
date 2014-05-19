-module(dragontiger_player).
-behavior(gen_server).
-include("dragontiger_game_eventbus.hrl").
-include("user.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([bet/3,start_link/3]).

start_link(Table,User,Payout)->
	gen_server:start_link(?MODULE,{Table,User,Payout},[]).

bet(Pid,Cats,Amounts)->
	gen_server:call(Pid,{bet,Cats,Amounts}).

-record(state,{game,table,user,payout}).


init({Table,User=#user{id=UserId},Payout})->
	?ADD_HANDLER(Table,UserId,self()),
	{ok,#state{table=Table,user=User,payout=Payout}}.

handle_call(_Event={bet,Cats,Amounts},_From,State)->
	Result=baccarat_game_api:bet(Cats,Amounts),
	{reply,Result,State}.

handle_cast(Request,State)->
	lager:error("unexpected Request ~p, State ~p",[Request,State]),
	{noreply,State}.

handle_info({json,Json},State)->
	lager:info("json ~p, state ~p",[Json,State]),
	{noreply,State};
handle_info(Info,State)->
	lager:error("unexpected Info ~p, State ~p",[Info,State]),
	{noreply,State}.

terminate(Reason,State=#state{table=Table,user=#user{id=UserId}})->
	lager:info("terminate, Reason ~p, State ~p",[Reason,State]),
	?DELETE_HANDLER(Table,UserId,Reason),
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.
