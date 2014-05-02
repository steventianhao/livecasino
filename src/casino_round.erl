-module(casino_round).
-behaviour(gen_server).

-record(table,{id}).
-record(game,{id,name}).
-record(shoe,{id,index,create_time}).
-record(round,{id,index,create_time,status=closed,cards,result,end_time}).
-record(dealer,{id,name,photo,actor}).
-record(bet,{player_id,bet_type,bet_amount}).

-record(state,{table,game,dealer,shoe,round}).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

new_table(Id)->
	#table{id=Id}.

new_game(Id,Name)->
	#game{id=Id,name=Name}.

new_dealer(Id,Name,Photo,Actor)->
	#dealer{id=Id,name=Name,photo=Photo,actor=Actor}.

dealer_connected(RoundPid,Dealer)->
	gen_server:call(RoundPid,{dealer_connected,Dealer}).

dealer_quit()->
	undefined.

dealer_new_shoe()->
	undefined.

dealer_start_bet()->
	undefined.

dealer_stop_bet()->
	undefined.

dealer_deal(pos,card)->
	undefined.

dealer_clear(pos)->
	undefined.

dealer_scan(card)->
	undefined.

dealer_commit(cards)->
	undefined.

player_connected(player)->
	undefined.

player_quit(player)->
	undefined.

player_bet(types,amounts)->
	undefined.


init([TableId,GameId,GameName])->
	State=#state{table=new_table(TableId),game=new_game(GameId,GameName)},
	{ok,State}.

handle_info(_Info,State)->
	{stop,normal,State}.

handle_call({dealer_connected,Dealer},From,State=#state{dealer=DealerNow})->
	{Result,NewState}=
		if
			DealerNow==undefined ->
				{ok,State#state{dealer=Dealer}};
			DealerNow#dealer.name==Dealer#dealer.name ->
				{ok,State#state{dealer=Dealer}};
			true ->
				{confict,State}
		end,
	{reply,Result,NewState}.

handle_cast(_Msg,State)->
	{noreply,State}.

terminate(_Reason,_State)->
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.








