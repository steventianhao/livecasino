-module(casino_bets).
-export([is_valid_bets/3,create_bet_req/5]).
-include("db.hrl").

is_valid_bet_cats(Cats,AllBetCats)->
	Cs=sets:from_list(Cats),
	L= sets:size(Cs),
	case length(Cats) of
		L->
			sets:is_subset(Cs,sets:from_list(AllBetCats));
		_ ->
			false
	end.
	
is_valid_bet_amounts(Amounts)->
	lists:all(fun(E)-> E>0 end,Amounts).
		
is_valid_bets(Cats=[C1|_],Amounts=[A1|_],AllBetCats=[_|_]) when is_integer(C1) andalso is_number(A1) andalso length(Cats)==length(Amounts)->
	is_valid_bet_cats(Cats,AllBetCats) andalso is_valid_bet_amounts(Amounts);
is_valid_bets(_,_,_)->
	false.

create_bet_req(RoundId,UserId,TableId,Cats,Amounts)->
	Cstr = string:join([integer_to_list(C) || C <-Cats],","),
	Astr = string:join([float_to_list(A,[{decimals,2}]) || A <-Amounts],","),	
	Total = lists:sum(Amounts),
 	#db_bet_req{round_id=RoundId,player_id=UserId,player_table_id=TableId,bet_cats=Cstr,bet_amounts=Astr,total_amount=Total}.
