-module(player_bets).
-export([is_valid_bets/3]).


is_valid_cats(Cats,CL,AllCats)->
	Cs=sets:from_list(Cats),
	case sets:size(Cs) of
		CL->
			sets:is_subset(Cs,sets:from_list(AllCats));
		_ ->
			false
	end.
	
is_valid_amounts(Amounts)->
	lists:all(fun(E)-> E>0 end,Amounts).
		
is_valid_bets(Cats=[C1|_],Amounts=[A1|_],AllCats) when is_integer(C1) andalso is_number(A1)->
	CL=length(Cats),
	case length(Amounts) of
		CL->
			is_valid_amounts(Amounts) andalso is_valid_cats(Cats,CL,AllCats);
		_ ->
		 	false
	end;
	 
is_valid_bets(_,_,_)->
	false.