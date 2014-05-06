-module(baccarat_game).
-behavior(gen_fsm).

%% gen_fsm callbacks
-export([init/1,code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,terminate/3]).

%% all states transitions
-export([stopped/3,dealing/3,betting/3]).

%% API
-export([start_link/0,start_bet/0,stop_bet/0,commit/1]).

-define(TICK,15).

-record(state,{dealer,table,round,ticker}).

start_link()->
	gen_fsm:start_link({local,?MODULE},?MODULE,[],[]).

init([])->
	{ok,stopped,#state{}}.

start_bet()->
	gen_fsm:sync_send_event(?MODULE,start_bet).

stop_bet()->
	gen_fsm:sync_send_event(?MODULE,stop_bet).

commit(Cards)->
	gen_fsm:sync_send_event(?MODULE,{commit,Cards}).

stopped(start_bet,_From,State=#state{ticker=Ticker})->
	%%start to count down
	case Ticker of
		undefined-> ok;
		{TRef,_} -> timer:cancel(TRef)
	end,
	{ok,TRef2}=timer:send_interval(1000,tick),
	NewState=State#state{ticker={TRef2,?TICK}},
	{reply,ok,betting,NewState};
stopped(Event,_From,State)->
	lager:error("unexpected event when stopped, event ~p,state ~p",[Event,State]),
	{reply,error_status,stopped,State}.

betting(stop_bet,_From,State=#state{ticker=Ticker})->
	%% if the count down still there, cancel it
	case Ticker of
		undefined-> ok;
		{TRef,_}-> timer:cancel(TRef)
	end,
	NewState=State#state{ticker=undefined},
	{reply,ok,dealing,NewState};
betting(Event,_From,State)->
	lager:error("unexpected event when betting, event ~p,state ~p",[Event,State]),
	{reply,error_status,betting,State}.	

dealing(Event={commit,_Cards},_From,State)->
	lager:info("dealing, Event ~p, State ~p",[Event,State]),
	{reply,ok,stopped,State};
dealing(Event,_From,State)->
	lager:error("unexpected event when dealing, event ~p,state ~p",[Event,State]),
	{reply,error_status,dealing,State}.	

handle_info(tick,betting,State=#state{ticker=Ticker})->
	%%send the tick to all players intrested in
	lager:info("handle_tick when betting, state ~p",[State]),
	case Ticker of
		{TRef,0} -> 
			timer:cancel(TRef),
			NewState=State#state{ticker=undefined},
			{next_state,dealing,NewState};
		{TRef,Value}->
			NewState=State#state{ticker={TRef,Value-1}},
			{next_state,betting,NewState}
	end;

handle_info(Info,StateName,State)->
	lager:error("unexpected handle_info, info ~p,stateName ~p,state ~p",[Info,StateName,State]),
	{next_state,StateName,State}.

handle_event(Event,StateName,State)->
	lager:error("unexpected handle_event, event ~p,stateName ~p,state ~p",[Event,StateName,State]),
	{next_state,StateName,State}.

handle_sync_event(Event,From,StateName,State)->
	lager:error("unexpected handle_sync_event, event ~p,from ~p,info ~p,stateName ~p,state ~p",[Event,From,StateName,State]),
	{next_state,StateName,State}.

terminate(Reason,StateName,State)->
	lager:info("terminate, reason ~p,stateName ~p,state ~p",[Reason,StateName,State]),
	ok.

code_change(OldVsn,StateName,State,Extra)->
	lager:info("code_change oldVsn ~p,stateName ~p,state ~p,extra ~p",[OldVsn,StateName,State,Extra]),
	{ok,StateName,State}.