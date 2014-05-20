-module(dragontiger_player).
-behavior(gen_server).
-include("user.hrl").

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([bet/3,start_link/4]).
-record(state,{table,user,server,eventbus}).


start_link(Server,EventBus,Table,User) when is_pid(Server) andalso is_pid(EventBus) andalso is_integer(Table) andalso is_record(User,user)->
	gen_server:start_link(?MODULE,{Server,EventBus,Table,User},[]).

bet(Pid,Cats,Amounts)->
	gen_server:call(Pid,{bet,Cats,Amounts}).

init({Server,EventBus,Table,User=#user{id=UserId}})->
	game_eventbus:add_handler(EventBus,UserId,self()),
	{ok,#state{table=Table,user=User,server=Server,eventbus=EventBus}}.

handle_call(_Event={bet,Cats,Amounts},_From,State=#state{server=Server})->
	Result=dragontiger_game_api:bet(Server,Cats,Amounts),
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

terminate(Reason,State=#state{user=#user{id=UserId},eventbus=EventBus})->
	lager:info("terminate, Reason ~p, State ~p",[Reason,State]),
	game_eventbus:del_handler(EventBus,UserId,Reason),
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.