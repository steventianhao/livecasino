-module(casino_card).
-include("card.hrl").

-export([string_to_cards/1,card_to_string/1,cards_to_string/1,one_card/1]).
-export([is_pair/2,total/2]).
-export([put/4,remove/2]).

put(Pos,Card,Cards,AllPos)->
	case lists:member(Pos,AllPos) of
		true->
			{ok,maps:put(Pos,Card,Cards)};
		false->
			error
	end.

remove(Pos,Cards)->
	case maps:is_key(Pos,Cards) of
		true -> 
			{ok,maps:remove(Pos,Cards)};
		false->
			error
	end.

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

one_card(Card)->
	[S,R]=binary_to_list(Card),
	#card{suit=S,rank=R}.