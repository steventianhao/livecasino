-module(baccarat_game).
-behavior(gen_fsm).

%% gen_fsm callbacks
-export([init/1,code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,terminate/3]).

%% all states transitions
-export([stopped/3,dealing/3,betting/3]).
-include("round.hrl").

-define(GAME,baccarat).
%% API

-record(state,{dealer,table,ticker,cards,countdown,round}).

patch_lager(State)->
	case State#state.cards of
		undefined-> 
			State;
		CardsMap->
			State#state{cards=maps:to_list(CardsMap)}
	end.

init({Countdown,Table,Round})->
	StateName=case Round of
		undefined -> 
			stopped;
		_->
			case Round#round.status of
				?BETTING -> 
					betting;
				?DEALING -> 
					dealing;
				?DONE -> 
					stopped
			end
	end,
	{ok,StateName,#state{countdown=Countdown,table=Table,round=Round}}.

checkDealer(DealerNow,Pid,Fun1,Fun2)->
	case DealerNow of
		{Pid,_} -> Fun1();
		_ -> Fun2()
	end.

stopped(new_shoe,{Pid,_},State=#state{dealer=DealerNow,round=Round})->
	lager:info("stopped#new_shoe,state ~p",[patch_lager(State)]),
	Fun1 = fun()->
			NewRound=baccarat_round:new_shoe(Round),
			NewState=State#state{round=NewRound},
			{reply,ok,stopped,NewState}
		end,
	Fun2= fun()-> {reply,error_channel,stopped,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);

stopped(start_bet,{Pid,_},State=#state{countdown=Countdown,dealer=DealerNow,round=Round})->
	lager:info("stopped#start_bet,state ~p",[patch_lager(State)]),
	Fun1 = fun()->
			case Round of
				undefined ->
					{reply,{error,need_new_shoe},stopped,State};
				_ ->
					NewRound=baccarat_round:set_betting(Round,DealerNow),
					{ok,TRef}=timer:send_interval(1000,tick),
					NewState=State#state{ticker={TRef,Countdown},cards=#{},round=NewRound},
					{reply,ok,betting,NewState}
			end
		end,
	Fun2= fun()-> {reply,error_channel,stopped,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);
	
stopped(Event,_From,State)->
	lager:error("unexpected event when stopped, event ~p,state ~p",[Event,patch_lager(State)]),
	{reply,unexpected,stopped,State}.
	
betting(stop_bet,{Pid,_},State=#state{ticker=Ticker,dealer=DealerNow,round=Round})->
	lager:info("betting#stop_bet,state ~p",[patch_lager(State)]),
	Fun1 = fun() ->
			case Ticker of
				undefined-> ok;
				{TRef,_}-> timer:cancel(TRef)
			end,
			NewRound=baccarat_round:set_dealing(Round),
			NewState=State#state{ticker=undefined,round=NewRound},
			{reply,ok,dealing,NewState}
		end,
	Fun2= fun()-> {reply,error_channel,betting,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);

betting(Event,_From,State)->
	lager:error("unexpected event when betting, event ~p,state ~p",[Event,patch_lager(State)]),
	{reply,unexpected,betting,State}.

dealing(Event={deal,Pos,Card},{Pid,_},State=#state{cards=Cards,dealer=DealerNow})->
	lager:info("dealing#deal, Event ~p, State ~p",[Event,patch_lager(State)]),
	Fun1 = fun()->
		case baccarat_dealer_mod:put(Pos,Card,Cards) of
			{ok,NewCards} ->
				NewState=State#state{cards=NewCards},
				{reply,ok,dealing,NewState};
			error ->
				{reply,error,dealing,State}
		end
	end,
	Fun2 = fun()-> {reply,error_channel,dealing,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);

dealing(Event={scan,Card},{Pid,_},State=#state{cards=Cards,dealer=DealerNow})->
	lager:info("dealing#scan, Event ~p, State ~p",[Event,patch_lager(State)]),
	Fun1 = fun()->
		case baccarat_dealer_mod:add(Card,Cards) of
			{error,_} ->
				{reply,error,dealing,State};
			{Status,Pos,NewCards}->
				{reply,{Status,Pos},dealing,State#state{cards=NewCards}}
		end
	end,
	Fun2 = fun()-> {reply,error_channel,dealing,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);

dealing(Event={clear,Pos},{Pid,_},State=#state{cards=Cards,dealer=DealerNow})->
	lager:info("dealing#clear, Event ~p, State ~p",[Event,patch_lager(State)]),
	Fun1 = fun()->
		case baccarat_dealer_mod:remove(Pos,Cards) of
			{ok,NewCards} ->
				NewState=State#state{cards=NewCards},
				{reply,ok,dealing,NewState};
			error ->
				{reply,error,dealing,State}
		end
	end,
	Fun2 = fun()-> {reply,error_channel,dealing,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);

dealing(commit,{Pid,_},State=#state{cards=Cards,dealer=DealerNow,round=Round})->
	lager:info("dealing#commit, State ~p",[patch_lager(State)]),
	%%check the cards are valid in accordence with the game rule
	Fun1 = fun()->
		case baccarat_dealer_mod:validate(Cards) of
			true->
				NewRound=baccarat_round:set_done(Round,Cards),
				NewState=State#state{round=NewRound},
				{reply,ok,stopped,NewState};
			false->
				{reply,error,dealing,State}
		end
	end,
	Fun2 = fun()-> {reply,error_channel,dealing,State} end,
	checkDealer(DealerNow,Pid,Fun1,Fun2);

dealing(Event,_From,State)->
	lager:error("unexpected event when dealing, event ~p,state ~p",[Event,patch_lager(State)]),
	{reply,unexpected,dealing,State}.


handle_info(tick,betting,State=#state{ticker=Ticker})->
	%%send the tick to all players intrested in
	lager:info("handle tick when betting, state ~p",[patch_lager(State)]),
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
	lager:error("handle dealer process DOWN, info ~p,stateName ~p,state ~p",[Info,StateName,patch_lager(State)]),
	case DealerNow of
		{Pid,_Dealer}->
			NewState=State#state{dealer=undefined},
			{next_state,StateName,NewState};
		_ ->
			{next_state,StateName,State}
	end;	
handle_info(Info,StateName,State)->
	lager:error("unexpected handle info, info ~p,stateName ~p,state ~p",[Info,StateName,patch_lager(State)]),
	{next_state,StateName,State}.

handle_event(Event={dealer_disconnect,Pid},StateName,State=#state{dealer=DealerNow})->
	lager:info("unexpected handle_event, event ~p,stateName ~p,state ~p",[Event,StateName,patch_lager(State)]),
	NewState=case DealerNow of
		{Pid,_Dealer} ->
			State#state{dealer=undefined};
		_ -> 
			State
	end,
	{next_state,StateName,NewState};
handle_event(Event,StateName,State)->
	lager:error("unexpected handle_event, event ~p,stateName ~p,state ~p",[Event,StateName,patch_lager(State)]),
	{next_state,StateName,State}.

handle_sync_event(Event={update_countdown,Countdown},From,StateName,State)->
	lager:info("update_countdown, event ~p,from ~p,stateName ~p,state ~p",[Event,From,StateName,patch_lager(State)]),
	if 
		Countdown > 0 -> 
			NewState=State#state{countdown=Countdown},
			{reply,ok,StateName,NewState};
		true ->
			{reply,error,StateName,State}
	end;

handle_sync_event(Event={dealer_connect,Dealer},From={Pid,_},StateName,State=#state{dealer=DealerNow})->
	lager:info("dealer_connected, event ~p,from ~p,stateName ~p,state ~p",[Event,From,StateName,patch_lager(State)]),
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
	lager:error("unexpected handle_sync_event, event ~p,from ~p,stateName ~p,state ~p",[Event,From,StateName,patch_lager(State)]),
	{next_state,StateName,State}.

terminate(Reason,StateName,State)->
	lager:info("terminate, reason ~p,stateName ~p,state ~p",[Reason,StateName,patch_lager(State)]),
	ok.

code_change(OldVsn,StateName,State,Extra)->
	lager:info("code_change oldVsn ~p,stateName ~p,state ~p,extra ~p",[OldVsn,StateName,patch_lager(State),Extra]),
	{ok,StateName,State}.