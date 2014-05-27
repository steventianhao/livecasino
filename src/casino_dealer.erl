-module(casino_dealer).
-behavior(gen_server).

-compile([{parse_transform, lager_transform}]).

%%API
-export([start_link/1,stop/1,new_dealer/2,enter_table/2,quit_table/1,start_bet/1,stop_bet/1]).

%% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-include("dealer.hrl").

-record(state, {dealer,table}).

-define(DEALER(Name),"dealer_"++Name).
-define(PID(Name),gproc:where({n,l,"dealer_"++Name})).

new_dealer(Id,Name)->
	#dealer{id=Id,name=Name}.

start_link(Dealer)->
	gen_server:start_link(?MODULE,Dealer,[]).

init(Dealer=#dealer{name=Name})->
	process_flag(trap_exit,true),
	true = gproc:reg({n,l,?DEALER(Name)},Dealer),
	{ok,#state{dealer=Dealer}}.

stop(Name)->
	gen_server:cast(?PID(Name),stop).

enter_table(Name,TableId)->
	gen_server:call(?PID(Name),{enter_table,TableId}).

quit_table(Name)->
	gen_server:call(?PID(Name),quit_table).

start_bet(Name)->
	gen_server:call(?PID(Name),start_bet).

stop_bet(Name)->
	gen_server:call(?PID(Name),stop_bet).

handle_cast(stop,State)->
	{stop,normal,State}.
handle_call(stop_bet,_From,State=#state{table=Table})->
	Result=case Table of
		undefined-> dealer_status_error;
		TableId->
			UniqueTable=lists:concat(["table_",TableId]),
			Pid=gproc:where({n,l,UniqueTable}),
			case Pid of
				undefined->table_process_not_exist;
				_-> gen_server:call(Pid,stop_bet)
			end
	end,
	{reply,Result,State};
handle_call(start_bet,_From,State=#state{table=Table})->
	Result=case Table of
		undefined->dealer_status_error;
		TableId->
			%%look the table process, send message to it.
			UniqueTable=lists:concat(["table_",TableId]),
			Pid=gproc:where({n,l,UniqueTable}),
			case Pid of
				undefined-> table_process_not_exist;
				_->gen_server:call(Pid,start_bet)
			end
	end,
	{reply,Result,State};
handle_call({enter_table,TableId},_From,State=#state{table=Table})->
	UniqueTableDealer=lists:concat(["dealer_table_",TableId]),
	%% should check the table process exist or not.
	%% should check there's no other dealer connected to that table
	{Result,NewState}=case Table of
		undefined->
			{Pid,_}=gproc:reg_or_locate({n,l,UniqueTableDealer}),
			if 
				self()==Pid->
					{ok,State#state{table=TableId}};
				true ->
					{existed_dealer,State}
			end;
		_TableOn -> 
			{existed_table,State}
	end,
	{reply,Result,NewState};
handle_call(quit_table,_From,State=#state{table=Table})->
	NewState=case Table of
		undefined-> 
			State;
		TableId ->
			UniqueTableDealer=lists:concat(["dealer_table_",TableId]),
			gproc:unreg({n,l,UniqueTableDealer}),
			State#state{table=undefined}
	end,
	{reply,ok,NewState};
handle_call(_Request,_From,State)->
	{stop,unexpected,State}.

handle_info(Info={'EXIT',_From,_Reason},State)->
	lager:debug("in handle_info ~p",[Info]),
	{stop,normal,State}.

terminate(Reason,State)->
	lager:debug("in terminate reson ~p, state ~p",[Reason,State]),
	gproc:goodbye(),
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.