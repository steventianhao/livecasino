-module(dragontiger_game).
-behavior(gen_fsm).

%% gen_fsm callbacks
-export([init/1,code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,terminate/3]).

%% all states transitions
-export([stopped/3,dealing/3,betting/3]).
-include("round.hrl").
-include("db.hrl").
-include("dealer.hrl").

%% API
-define(GAME,dragontiger).

-define(GAME_ROUND,dragontiger_round).
-define(GAME_DEALER_MOD,dragontiger_dealer_mod).
-define(CASINO_DB,mysql_casino_master).

-record(state,{dealer,table,ticker,cards,countdown,round,eventbus}).

init({EventBus,Table,Countdown})->
	{ok,stopped,#state{countdown=Countdown,table=Table,eventbus=EventBus}}.

stopped(new_shoe,{Pid,_},State=#state{dealer={Pid,_},round=Round,table=Table,eventbus=EventBus})->
	lager:info("stopped#new_shoe,state ~p",[State]),
	NewRound=?GAME_ROUND:new_shoe(Round),
	NewState=State#state{round=NewRound},
	gen_event:notify(EventBus,{new_shoe,Table,NewRound}),
	{reply,ok,stopped,NewState};

stopped(start_bet,{Pid,_},State=#state{dealer={Pid,_Dealer},round=undefined})->
	lager:info("stopped#start_bet,state ~p",[State]),
	{reply,{error,need_new_shoe},stopped,State};

stopped(start_bet,{Pid,_},State=#state{countdown=Countdown,dealer={Pid,Dealer},round=Round,table=Table,eventbus=EventBus})->
	lager:info("stopped#start_bet,state ~p",[State]),
	NewRound=?GAME_ROUND:new_round(Round),
	%%-record(round,{id,dealer,shoeIndex,roundIndex,cards,createTime,finishTime,status}).
	#round{createTime={Mills,_},roundIndex=RoundIndex,shoeIndex=ShoeIndex}=NewRound,
	DealerId=Dealer#dealer.id,
	DbNewRound=#db_new_round_req{shoe_index=ShoeIndex,round_index=RoundIndex,dealer_id=DealerId,dealer_table_id=Table,create_time=Mills},
	NewRoundId=mysql_db:insert_round(?CASINO_DB,DbNewRound),
	NewRound2=NewRound#round{id=NewRoundId},
	TRef=erlang:send_after(1000,self(),tick),
	NewState=State#state{ticker={TRef,Countdown},cards=#{},round=NewRound2},
	gen_event:notify(EventBus,{start_bet,Table,NewRound,Countdown}),
	{reply,ok,betting,NewState};

stopped(Event,_From,State)->
	lager:error("unexpected event when stopped, event ~p,state ~p",[Event,State]),
	{reply,unexpected,stopped,State}.



betting(Event={try_bet,_Cats,_Amounts},{_Pid,Tag},State=#state{round=Round})->
	lager:info("bet Event ~p,State ~p",[Event,State]),
	%% add the bets into the limit table, check the limits, then return ok
	{reply,{ok,Tag,Round#round.id},betting,State};
	
betting(stop_bet,{Pid,_},State=#state{ticker={TRef,_},dealer={Pid,_Dealer},round=Round,table=Table,eventbus=EventBus})->
	lager:info("betting#stop_bet,state ~p",[State]),
	erlang:cancel_timer(TRef),
	NewState=State#state{ticker=undefined},
	Mills=casino_utils:mills(),
	1=mysql_db:update_round(?CASINO_DB,Round#round.id,Mills),
	gen_event:notify(EventBus,{stop_bet,Table}),
	{reply,ok,dealing,NewState};

betting(Event,_From,State)->
	lager:error("unexpected event when betting, event ~p,state ~p",[Event,State]),
	{reply,unexpected,betting,State}.

dealing(Event={deal,Pos,Card},{Pid,_},State=#state{cards=Cards,dealer={Pid,_},eventbus=EventBus})->
	lager:info("dealing#deal, Event ~p, State ~p",[Event,State]),
	NewCards=?GAME_DEALER_MOD:put(Pos,Card,Cards),
	NewState=State#state{cards=NewCards},
	gen_event:notify(EventBus,{deal,Pos,Card}),
	{reply,ok,dealing,NewState};
		

dealing(Event={scan,Card},{Pid,_},State=#state{cards=Cards,dealer={Pid,_},table=Table,eventbus=EventBus})->
	lager:info("dealing#scan, Event ~p, State ~p",[Event,State]),
	case ?GAME_DEALER_MOD:add(Card,Cards) of
		{error,_} ->
			{reply,error,dealing,State};
		{Status,Pos,NewCards}->
			gen_event:notify(EventBus,{deal,Table,Pos,Card}),
			{reply,{Status,Pos},dealing,State#state{cards=NewCards}}
	end;


dealing(Event={clear,Pos},{Pid,_},State=#state{cards=Cards,dealer={Pid,_},table=Table,eventbus=EventBus})->
	lager:info("dealing#clear, Event ~p, State ~p",[Event,State]),
	case ?GAME_DEALER_MOD:remove(Pos,Cards) of
		{ok,NewCards} ->
			NewState=State#state{cards=NewCards},
			gen_event:notify(EventBus,{clear,Table,Pos}),
			{reply,ok,dealing,NewState};
		error ->
			{reply,error,dealing,State}
	end;
	
dealing(commit,{Pid,_},State=#state{cards=Cards,dealer={Pid,_},round=Round,table=Table,eventbus=EventBus})->
	lager:info("dealing#commit, State ~p",[State]),
	%%check the cards are valid in accordence with the game rule
	case baccarat_dealer_mod:validate(Cards) of
		true->
			Mills=casino_utils:mills(),
			Cstr=?GAME_DEALER_MOD:to_string(Cards),
			1=mysql_db:update_round(?CASINO_DB,Round#round.id,Cstr,Mills),
			gen_event:notify(EventBus,{commit,Table,Round,Cards}),				
			{reply,ok,stopped,State};
		false->
			{reply,error,dealing,State}
	end;


dealing(Event,_From,State)->
	lager:error("unexpected event when dealing, event ~p,state ~p",[Event,State]),
	{reply,unexpected,dealing,State}.


handle_info(tick,betting,State=#state{ticker=Ticker,table=Table,eventbus=EventBus})->
	%%send the tick to all players intrested in
	lager:info("handle tick when betting, state ~p",[State]),
	case Ticker of
		{_,0} ->
			gen_event:notify(EventBus,{tick,Table,0}),
			{next_state,betting,NewState};
		{_,Value}->
			gen_event:notify(EventBus,{tick,Table,Value}),
			TRef=erlang:send_after(1000,self(),tick),
			NewState=State#state{ticker={TRef,Value-1}},
			{next_state,betting,NewState}
	end;
handle_info(Info={'DOWN',_Ref,process,Pid,_},StateName,State=#state{dealer={Pid,Dealer},table=Table,eventbus=EventBus})->
	lager:error("handle dealer process DOWN, info ~p,stateName ~p,state ~p",[Info,StateName,State]),
	NewState=State#state{dealer=undefined},
	gen_event:notify(EventBus,{dealer_disconnect,Table,Dealer}),
	{next_state,StateName,NewState};
	
handle_info(Info,StateName,State)->
	lager:error("unexpected handle info, info ~p,stateName ~p,state ~p",[Info,StateName,State]),
	{next_state,StateName,State}.

handle_event(Event={dealer_disconnect,Pid},StateName,State=#state{dealer={Pid,Dealer},table=Table,eventbus=EventBus})->
	lager:info("dealer_disconnect handle_event, event ~p,stateName ~p,state ~p",[Event,StateName,State]),
	gen_event:notify(EventBus,{dealer_disconnect,Table,Dealer}),
	NewState=State#state{dealer=undefined},
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
handle_sync_event(Event={dealer_connect,Dealer},From={Pid,_},StateName,State=#state{dealer=undefined,table=Table,eventbus=EventBus})->
	lager:info("dealer_connected, event ~p,from ~p,stateName ~p,state ~p",[Event,From,StateName,State]),
	NewState=State#state{dealer={Pid,Dealer}},
	erlang:monitor(process,Pid),
	gen_event:notify(EventBus,{dealer_connect,Table,Dealer}),
	{reply,ok,StateName,NewState};

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