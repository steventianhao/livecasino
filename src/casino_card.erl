-module(casino_card).
-include("card.hrl").

-export([check_cards/1,check_one_card/2]).

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

