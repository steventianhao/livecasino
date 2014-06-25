-module(casino_card).
-include("card.hrl").

-export([check_cards/1,check_one_card/2,set_suit/2,cards_to_string/1,total/1]).
-export([is_pair/2]).

check_cards([])->
	true;
check_cards([S,R|T])->
	case check_one_card(S,R) of
	 	true ->
			check_cards(T);
		_ -> 
			false
	end;
check_cards(_)->
	false.

check_one_card(S,R) when is_integer(S) andalso is_integer(R)->
	lists:member(S,?ALL_SUIT) andalso lists:member(R,?ALL_RANK);
check_one_card(_,_)->
	false.

set_suit(Suit,Card)->
	Card#card{suit=Suit}.

cards_to_string(Cards)->
	lists:flatmap(fun(C)->[C#card.suit,C#card.rank] end,Cards).

total(Cards) ->
	lists:foldl(fun(X,Sum)->X#card.value+Sum end,0,Cards) rem 10.

is_pair(#card{rank=R1},#card{rank=R2})->
	R1==R2.