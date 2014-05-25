-module(baccarat_game).
-behavior(gen_fsm).

%% gen_fsm callbacks
-export([init/1,code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,terminate/3]).

%% all states transitions
-export([stopped/3,dealing/3,betting/3]).
-include("round.hrl").

%% API
-define(GAME,baccarat).
-define(GAME_ROUND,baccarat_round).
-define(GAME_DEALER_MOD,baccarat_dealer_mod).

-record(state,{dealer,table,ticker,cards,countdown,round,eventbus}).

init({EventBus,Table,Countdown})->
	{ok,stopped,#state{countdown=Countdown,table=Table,eventbus=EventBus}}.

stopped(new_shoe,{Pid,_},State=#state{dealer={Pid,_},round=Round,eventbus=EventBus})->
	lager:info("stopped#new_shoe,state ~p",[State]),
	NewRound=?GAME_ROUND:new_shoe(Round),
	NewState=State#state{round=NewRound},
	gen_event:notify(EventBus,{new_shoe,NewRound}),
	{reply,ok,stopped,NewState};

stopped(start_bet,{Pid,_},State=#state{dealer={Pid,_},round=undefined})->
	lager:info("stopped#start_bet,state ~p",[State]),
	{reply,{error,need_new_shoe},stopped,State};

stopped(start_bet,{Pid,_},State=#state{countdown=Countdown,dealer={Pid,Dealer},round=Round,eventbus=EventBus})->
	lager:info("stopped#start_bet,state ~p",[State]),
	NewRound=?GAME_ROUND:set_betting(Round,Dealer),
	TRef=erlang:send_after(1000,self(),tick),
	NewState=State#state{ticker={TRef,Countdown},cards=#{},round=NewRound},
	gen_event:notify(EventBus,{start_bet,NewRound,Countdown}),
	{reply,ok,betting,NewState};
	
stopped(Event,_From,State)->
	lager:error("unexpected event when stopped, event ~p,state ~p",[Event,State]),
	{reply,unexpected,stopped,State}.


betting(Event={bet,_Cats,_Amounts},_From,State)->
	lager:info("bet Event ~p,State ~p",[Event,State]),
	{reply,ok,betting,State};
	
betting(stop_bet,{Pid,_},State=#state{ticker={TRef,_},dealer={Pid,_},round=Round,eventbus=EventBus})->
	lager:info("betting#stop_bet,state ~p",[State]),
	erlang:cancel_timer(TRef),
	NewRound=?GAME_ROUND:set_dealing(Round),
	NewState=State#state{ticker=undefined,round=NewRound},
	gen_event:notify(EventBus,{stop_bet,NewRound}),
	{reply,ok,dealing,NewState};

betting(Event,_From,State)->
	lager:error("unexpected event when betting, event ~p,state ~p",[Event,State]),
	{reply,unexpected,betting,State}.

dealing(Event={deal,Pos,Card},{Pid,_},State=#state{cards=Cards,dealer={Pid,_},eventbus=EventBus})->
	lager:info("dealing#deal, Event ~p, State ~p",[Event,State]),
	case ?GAME_DEALER_MOD:put(Pos,Card,Cards) of
		{ok,NewCards} ->
			NewState=State#state{cards=NewCards},
			gen_event:notify(EventBus,{deal,Pos,Card}),
			{reply,ok,dealing,NewState};
		error ->
			{reply,error,dealing,State}
	end;


dealing(Event={scan,Card},{Pid,_},State=#state{cards=Cards,dealer={Pid,_},eventbus=EventBus})->
	lager:info("dealing#scan, Event ~p, State ~p",[Event,State]),
	case ?GAME_DEALER_MOD:add(Card,Cards) of
		{error,_} ->
			{reply,error,dealing,State};
		{Status,Pos,NewCards}->
			gen_event:notify(EventBus,{deal,Pos,Card}),
			{reply,{Status,Pos},dealing,State#state{cards=NewCards}}
	end;


dealing(Event={clear,Pos},{Pid,_},State=#state{cards=Cards,dealer={Pid,_},eventbus=EventBus})->
	lager:info("dealing#clear, Event ~p, State ~p",[Event,State]),
	case ?GAME_DEALER_MOD:remove(Pos,Cards) of
		{ok,NewCards} ->
			NewState=State#state{cards=NewCards},
			gen_event:notify(EventBus,{clear,Pos}),
			{reply,ok,dealing,NewState};
		error ->
			{reply,error,dealing,State}
	end;


dealing(commit,{Pid,_},State=#state{cards=Cards,dealer={Pid,_},round=Round,eventbus=EventBus})->
	lager:info("dealing#commit, State ~p",[State]),
	%%check the cards are valid in accordence with the game rule
	case ?GAME_DEALER_MOD:validate(Cards) of
		true->
			NewRound=dragontiger_round:set_done(Round,Cards),
			NewState=State#state{round=NewRound},
			gen_event:notify(EventBus,{commit,Round,Cards}),				
			{reply,ok,stopped,NewState};
		false->
			{reply,error,dealing,State}
	end;


dealing(Event,_From,State)->
	lager:error("unexpected event when dealing, event ~p,state ~p",[Event,State]),
	{reply,unexpected,dealing,State}.


handle_info(tick,betting,State=#state{ticker=Ticker,eventbus=EventBus})->
	%%send the tick to all players intrested in
	lager:info("handle tick when betting, state ~p",[State]),
	
	case Ticker of
		{_,0} ->
			gen_event:notify(EventBus,{tick,0}),
			NewState=State#state{ticker=undefined},
			{next_state,dealing,NewState};
		{_,Value}->
			gen_event:notify(EventBus,{tick,Value}),
			TRef=erlang:send_after(1000,self(),tick),
			NewState=State#state{ticker={TRef,Value-1}},
			{next_state,betting,NewState}
	end;
handle_info(Info={'DOWN',_Ref,process,Pid,_},StateName,State=#state{dealer={Pid,Dealer},eventbus=EventBus})->
	lager:error("handle dealer process DOWN, info ~p,stateName ~p,state ~p",[Info,StateName,State]),
	NewState=State#state{dealer=undefined},
	gen_event:notify(EventBus,{dealer_disconnect,Dealer}),
	{next_state,StateName,NewState};
	
handle_info(Info,StateName,State)->
	lager:error("unexpected handle info, info ~p,stateName ~p,state ~p",[Info,StateName,State]),
	{next_state,StateName,State}.

handle_event(Event={dealer_disconnect,Pid},StateName,State=#state{dealer={Pid,Dealer},eventbus=EventBus})->
	lager:info("dealer_disconnect handle_event, event ~p,stateName ~p,state ~p",[Event,StateName,State]),
	gen_event:notify(EventBus,{dealer_disconnect,Dealer}),
	{next_state,StateName,State#state{dealer=undefined}};

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

handle_sync_event(Event={dealer_connect,Dealer},From={Pid,_},StateName,State=#state{dealer=undefined,eventbus=EventBus})->
	lager:info("dealer_connected, event ~p,from ~p,stateName ~p,state ~p",[Event,From,StateName,State]),
	erlang:monitor(process,Pid),
	gen_event:notify(EventBus,{dealer_connect,Dealer}),
	{ok,State#state{dealer={Pid,Dealer}}};

handle_sync_event(Event={dealer_connect,_Dealer},From,StateName,State)->
	lager:info("dealer_connected, event ~p,from ~p,stateName ~p,state ~p",[Event,From,StateName,State]),
	{reply,{error,dealer_existed},StateName,State};

handle_sync_event(Event,From,StateName,State)->
	lager:error("unexpected handle_sync_event, event ~p,from ~p,stateName ~p,state ~p",[Event,From,StateName,State]),
	{next_state,StateName,State}.

terminate(Reason,StateName,State)->
	lager:info("terminate, reason ~p,stateName ~p,state ~p",[Reason,StateName,State]),
	ok.

code_change(OldVsn,StateName,State,Extra)->
	lager:info("code_change oldVsn ~p,stateName ~p,state ~p,extra ~p",[OldVsn,StateName,State,Extra]),
	{ok,StateName,State}.