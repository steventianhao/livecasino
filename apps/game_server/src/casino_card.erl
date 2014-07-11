-module(casino_card).
-include("card.hrl").

-export([check_cards/1,check_one_card/2]).
-export([string_to_cards/1,card_to_string/1,cards_to_string/1,one_card/1]).
-export([is_pair/2,total/2]).

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


card_to_string(#card{rank=N,suit=S})->
	[S,N].
cards_to_string(Cards)->
	lists:flatmap(fun(C)->card_to_string(C) end,Cards).

total(Cards,Func) ->
	lists:foldl(fun(X,Sum)->Func(X)+Sum end,0,Cards) rem 10.

is_pair(#card{rank=R1},#card{rank=R2})->
	R1==R2.

string_to_cards([])->
	[];
string_to_cards([S,R|T])->
	[#card{suit=S,rank=R}|string_to_cards(T)].

one_card([S,R])->
	#card{suit=S,rank=R}.