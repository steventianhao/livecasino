-module(dragontiger_dealer_mod).
-include("dragontiger.hrl").
-export([put/3,remove/2,add/2,from_string/1,to_string/1]).


put(Pos,Card,Cards) when is_map(Cards) andalso is_record(Card,card) andalso is_integer(Pos)->
	case lists:member(Pos,?ALL_POS) of
		true->
			{ok,maps:put(Pos,Card,Cards)};
		false->
			error
	end.

remove(Pos,Cards) when is_map(Cards) andalso is_integer(Pos)->
	case maps:is_key(Pos,Cards) of
		true -> 
			{ok,maps:remove(Pos,Cards)};
		false->
			error
	end.

add(Card,Cards) when map_size(Cards)==0->
	{more,?DRAGON_POS, Cards#{?DRAGON_POS=>Card}};
add(Card,Cards=#{?DRAGON_POS := _}) when map_size(Cards)==1->
	{done,?TIGER_POS,Cards#{?TIGER_POS=>Card}};
add(_Card,Cards)->
	{error,Cards}.

string_to_card([S,N],CardsMap)->
	C=maps:get(N,CardsMap), 
	C#card{suit=S}.
card_to_string(#card{name=N,suit=S})->
	[S,N].

from_string(Cards)->
	[Dc,Tc]=string:tokens(Cards,"#"),
	CardsMap=?CARDS_MAP,
	#{?DRAGON_POS=>string_to_card(Dc,CardsMap),?TIGER_POS=>string_to_card(Tc,CardsMap)}.


to_string(#{?DRAGON_POS :=D,?TIGER_POS :=T})->
	lists:append([card_to_string(D),"#",card_to_string(T)]).
