-module(player_bets).
-export([is_valid_bets/3]).

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
		
is_valid_bets(Cats=[C1|_],Amounts=[A1|_],AllBetCats) when is_integer(C1) andalso is_number(A1) andalso length(Cats)==length(Amounts)->
	is_valid_bet_amounts(Amounts) andalso is_valid_bet_cats(Cats,AllBetCats);
is_valid_bets(_,_,_)->
	false.