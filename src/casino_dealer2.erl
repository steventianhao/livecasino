-module(casino_dealer2).
-behaviour(gen_fsm).

-record(state,{name,table}).

-export([start_link/0,stop/1,login/3,logout/1,enter_table/2,quit_table/1]).
-export([init/1,anonymous/3,authenticated/3]).
-export([code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,terminate/3]).

start_link()->
	gen_fsm:start_link(?MODULE,[],[]).

init([])->
	{ok,anonymous,#state{}}.

stop(Pid)->
	gen_fsm:send_all_state_event(Pid,stop).

login(Pid,Name,Password)->
	gen_fsm:sync_send_event(Pid,{login,Name,Password}).

logout(Pid)->
	gen_fsm:sync_send_event(Pid,logout).

enter_table(Pid,TableId)->
	gen_fsm:sync_send_event(Pid,{enter_table,TableId}).

quit_table(Pid)->
	gen_fsm:sync_send_event(Pid,quit_table).

anonymous({login,Name,Password},_From,State)->
	case {Name,Password} of
		{"simon","123456"} ->
			io:format("simon123456 anonymous login ~p~p~n",[Name,Password]), 
			{reply,ok,authenticated,State#state{name=Name}};
		_Wrong -> 
			io:format("anonymous login ~p~p~n",[Name,Password]),
			{reply,error,anonymous,State}
	end.

authenticated({enter_table,TableId},_From,State=#state{table=Table})->
	io:format("enter_table ~p~n",[TableId]),
	case {Table,TableId} of
		{undefined,1} -> {reply,ok,authenticated,State#state{table=TableId}};
		_Wrong->{reply,error,authenticated,State}
	end;
authenticated(quit_table,_From,State=#state{table=Table})->
	io:format("quit_table ~p~n",[Table]),
	if 
		Table==undefined -> {reply,ok,authenticated,State};
		true -> {reply,ok,authenticated,State#state{table=undefined}}
	end;
authenticated(logout,_From,_State)->
	io:format("logout ~n"),
	{reply,ok,anonymous,#state{}}.

handle_event(stop,_StateName,State)->
	io:format("handle_event ~n"),
	{stop,normal,State}.
handle_sync_event(_Event,_From,_StateName,State)->
	io:format("handle_sync_event ~n"),
	{stop,normal,State}.

terminate(Reason,StateName,#state{table=TableId})->
	io:format("in the terminate, the reason is ~p, and the state is ~p~n",[Reason,StateName]),
	if
		TableId==undefined -> ok;
		true -> io:format("quit from table ~p~n",[TableId])
	end.

handle_info(_Info,_StateName,State)->
	io:format("handle_info ~n"),
	{stop,normal,State}.

code_change(_OldVsn,StateName,State,_Extra)->
	io:format("code_change ~n"),
	{ok,StateName,State}.