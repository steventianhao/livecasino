-module(dragontiger_player_mod).


-export([all_bet_cats/0,is_valid_bets/2,bets_to_string/2]).

-define(BET_CAT_DRAGON,2000).
-define(BET_CAT_TIGER,2001).
-define(BET_CAT_TIE,2002).
-define(BET_CAT_DRAGON_ODD,2003).
-define(BET_CAT_TIGER_ODD,2004).
-define(BET_CAT_DRAGON_EVEN,2005).
-define(BET_CAT_TIGER_EVEN,2006).

-define(BET_CATS_MAP,#{
	?BET_CAT_DRAGON => dragon,
	?BET_CAT_TIGER => tiger,
	?BET_CAT_TIE => tie,
	?BET_CAT_DRAGON_ODD => dragon_odd,
	?BET_CAT_TIGER_ODD => tiger_odd,
	?BET_CAT_DRAGON_EVEN => dragon_even,
	?BET_CAT_TIGER_EVEN => tiger_even
	}).

-define(ALL_BET_CATS,[?BET_CAT_DRAGON,?BET_CAT_TIGER,?BET_CAT_TIE,?BET_CAT_DRAGON_ODD,?BET_CAT_TIGER_ODD,?BET_CAT_DRAGON_EVEN,?BET_CAT_TIGER_EVEN]).

all_bet_cats()->
	sets:from_list(?ALL_BET_CATS).

is_valid_bet_cats(Cats)->
	Cs=sets:from_list(Cats),
	L= sets:size(Cs),
	case length(Cats) of
		L->
			sets:is_subset(Cs,all_bet_cats());
		_ ->
			false
	end.
is_valid_bet_amounts(Amounts)->
	lists:all(fun(E)-> E>0 end,Amounts).
	
total_amount(Amounts)->
	lists:sum(Amounts).
	
is_valid_bets(Cats=[C1|_],Amounts=[A1|_]) when is_integer(C1) andalso is_number(A1) andalso length(Cats)==length(Amounts)->
	is_valid_bet_cats(Cats) andalso is_valid_bet_amounts(Amounts);
is_valid_bets(_,_)->
	false.

bets_to_string(Cats,Amounts)->
	{string:join(Cats,","),string:join(Amounts,",")}.	


	
