-module(baccarat_player).

%% payout, for example, you can enter a baccarat with non-commission or commission, two options.
-record(range,{min,max}).
-record(table_limit,{id,range}).
-record(bet_limit,{type,range}).

%% limit.bets is a map of type=>range
-record(player_limit,{table,bets=#{}}).

%% limit is the player_limit
-record(state,{game,table,user,limits,payout}).


-define(GAME_SERVER,{local,baccarat_game}).

init({Game,Table,User,Limits,Payout})->
	%% add the listener handler
	{ok,#state{game=Game,table=Table,user=User,limits=Limits,payout=Payout}}.

check_bets(Cats,Amounts,BetLimits)->
	Fun = fun({C,A})->
			Range=maps:get(C,BetLimits),
			A >= Range#range.min andalso A =< Range#range.max
	end,
	lists:all(Fun,lists:zip(Cats,Amounts)).

handle_call(Event={bet,Cats,Amounts},_From,State=#state{limits=Limits})->
	case check_bets(Cats,Amounts,Limits#player_limit.bets) of
		true ->
			case gen_fsm:sync_send_all_state_event(?GAME_SERVER,Event) of
				{ok,_Round} -> 
					%% do the database transaction, deduct the money,save the bets
					{reply,ok,State};
				Result={error,not_betting}->
					{reply,Result,State}
			end;
		_ ->
			{reply,{error,invalid_bets},State}
	end.

terminate(Reason,State)->
	%% remove the listener handler
	ok.

