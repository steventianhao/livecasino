-module(baccarat_game).
-behavior(gen_fsm).

%% gen_fsm callbacks
-export([init/1,code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,terminate/3]).

%% all states transitions
-export([stopped/3,dealing/3,betting/3]).

%% API

-record(state,{dealer,table,ticker,cards,countdown}).

init(Countdown)->
	{ok,stopped,#state{countdown=Countdown}}.

checkDealer(DealerNow,Pid,Fun1,Fun2)->
	case DealerNow of
		{Pid,_} -> Fun1();
		_ -> Fun2()
	end.

stopped(start_bet,{Pid,_},State=#state{countdown=Countdown,dealer=DealerNow})->
	Fun1 = fun()->
			{ok,TRef}=timer:send_interval(1000,tick),
			NewState=State#state{ticker={TRef,Countdown},cards=orddict:new()},
			{reply,ok,betting,NewState}
		end,
	Fun2= fun()-> {reply,error_channel,stopped,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);
	
stopped(Event,{Pid,_},State=#state{dealer=DealerNow})->
	lager:error("unexpected event when stopped, event ~p,state ~p",[Event,State]),
	Fun1 = fun()-> {reply,error_status,stopped,State} end,
	Fun2 = fun()-> {reply,error_channel,stopped,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2).

betting(stop_bet,{Pid,_},State=#state{ticker=Ticker,dealer=DealerNow})->
	%% if the count down still there, cancel it
	Fun1 = fun() ->
			case Ticker of
				undefined-> ok;
				{TRef,_}-> timer:cancel(TRef)
			end,
			NewState=State#state{ticker=undefined},
			{reply,ok,dealing,NewState}
		end,
	Fun2= fun()-> {reply,error_channel,betting,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);

betting(Event,{Pid,_},State=#state{dealer=DealerNow})->
	lager:error("unexpected event when betting, event ~p,state ~p",[Event,State]),
	Fun1=fun()->{reply,error_status,betting,State} end,
	Fun2= fun()-> {reply,error_channel,betting,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2).


dealing(Event={deal,Pos,Card},{Pid,_},State=#state{cards=Cards,dealer=DealerNow})->
	lager:info("dealing, Event ~p, State ~p",[Event,State]),
	Fun1 = fun()->
	%%check the pos and card to see if it's valid
		NewCards=orddict:store(Pos,Card,Cards),
		NewState=State#state{cards=NewCards},
		{reply,ok,dealing,NewState}
	end,
	Fun2 = fun()-> {reply,error_channel,dealing,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);

dealing(Event={clear,Pos},{Pid,_},State=#state{cards=Cards,dealer=DealerNow})->
	lager:info("dealing, Event ~p, State ~p",[Event,State]),
	Fun1 = fun()->
		NewCards=orddict:erase(Pos,Cards),
		NewState=State#state{cards=NewCards},
		{reply,ok,dealing,NewState}
	end,
	Fun2 = fun()-> {reply,error_channel,dealing,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);

dealing(commit,{Pid,_},State=#state{dealer=DealerNow})->
	lager:info("dealing, Event ~p, State ~p",[commit,State]),
	%%check the cards are valid in accordence with the game rule
	Fun1 = fun()-> {reply,ok,stopped,State} end,
	Fun2 = fun()-> {reply,error_channel,dealing,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);

dealing(Event,{Pid,_},State=#state{dealer=DealerNow})->
	lager:error("unexpected event when dealing, event ~p,state ~p",[Event,State]),
	Fun1 = fun() -> {reply,error_status,dealing,State} end,
	Fun2 = fun() -> {reply,error_channel,dealing,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2).


handle_info(tick,betting,State=#state{ticker=Ticker})->
	%%send the tick to all players intrested in
	lager:info("handle_info tick&betting, state ~p",[State]),
	case Ticker of
		{TRef,0} -> 
			timer:cancel(TRef),
			NewState=State#state{ticker=undefined},
			{next_state,dealing,NewState};
		{TRef,Value}->
			NewState=State#state{ticker={TRef,Value-1}},
			{next_state,betting,NewState}
	end;
handle_info(Info={'DOWN',_Ref,process,Pid,_},StateName,State=#state{dealer=DealerNow})->
	lager:error("handle dealer process DOWN, info ~p,stateName ~p,state ~p",[Info,StateName,State]),
	case DealerNow of
		{Pid,_Dealer}->
			NewState=State#state{dealer=undefined},
			{next_state,StateName,NewState};
		_ ->
			{next_state,StateName,State}
	end;	
handle_info(Info,StateName,State)->
	lager:error("unexpected handle_info, info ~p,stateName ~p,state ~p",[Info,StateName,State]),
	{next_state,StateName,State}.

handle_event(Event={dealer_disconnect,Pid},StateName,State=#state{dealer=DealerNow})->
	lager:info("unexpected handle_event, event ~p,stateName ~p,state ~p",[Event,StateName,State]),
	NewState=case DealerNow of
		{Pid,_Dealer} ->
			State#state{dealer=undefined};
		_ -> 
			State
	end,
	{next_state,StateName,NewState};
handle_event(Event,StateName,State)->
	lager:error("unexpected handle_event, event ~p,stateName ~p,state ~p",[Event,StateName,State]),
	{next_state,StateName,State}.

handle_sync_event(Event={update_countdown,Countdown},From,StateName,State)->
	lager:info("update_countdown, event ~p,from ~p,stateName ~p,state ~p",[Event,From,StateName,State]),
	if 
		Countdown > 0 -> 
			NewState=State#state{countdown=Countdown},
			{reply,ok,StateName,NewState};
		true ->
			{reply,error,StateName,State}
	end;

handle_sync_event(Event={dealer_connect,Dealer},From={Pid,_},StateName,State=#state{dealer=DealerNow})->
	lager:info("dealer_connected, event ~p,from ~p,stateName ~p,state ~p",[Event,From,StateName,State]),
	{Result,NewState}=case DealerNow of
		undefined->
		   	State2=State#state{dealer={Pid,Dealer}},
		   	erlang:monitor(process,Pid),
			{ok,State2};
		_ ->
			{{error,dealer_existed},State}
	end,
	{reply,Result,StateName,NewState};
handle_sync_event(Event,From,StateName,State)->
	lager:error("unexpected handle_sync_event, event ~p,from ~p,stateName ~p,state ~p",[Event,From,StateName,State]),
	{next_state,StateName,State}.

terminate(Reason,StateName,State)->
	lager:info("terminate, reason ~p,stateName ~p,state ~p",[Reason,StateName,State]),
	ok.

code_change(OldVsn,StateName,State,Extra)->
	lager:info("code_change oldVsn ~p,stateName ~p,state ~p,extra ~p",[OldVsn,StateName,State,Extra]),
	{ok,StateName,State}.