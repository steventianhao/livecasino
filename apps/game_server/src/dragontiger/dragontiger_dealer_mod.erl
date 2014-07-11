-module(dragontiger_dealer_mod).
-include("dragontiger.hrl").
-export([put/3,remove/2,add/2,from_string/1,to_string/1,validate/1,one_card/1]).

put(Pos,Card,Cards)->
	case lists:member(Pos,?ALL_POS) of
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

add(Card,Cards=#{}) when map_size(Cards)==0->
	{more,?DRAGON_POS, Cards#{?DRAGON_POS=>Card}};
add(Card,Cards=#{?DRAGON_POS := _}) when map_size(Cards)==1->
	{done,?TIGER_POS,Cards#{?TIGER_POS=>Card}};
add(_Card,Cards)->
	{error,Cards}.

validate(Cards=#{?DRAGON_POS := _, ?TIGER_POS := _}) when map_size(Cards)==2 -> 
	true;
validate(_)->
	false.
	
from_string(Cards)->
	[Ds,Ts]=string:tokens(Cards,"#"),
	Dc =casino_card:string_to_cards(Ds),
	Tc =casino_card:string_to_cards(Ts),
	#{?DRAGON_POS=>Dc,?TIGER_POS=>Tc}.

to_string(#{?DRAGON_POS :=D,?TIGER_POS :=T})->
	lists:append([casino_card:card_to_string(D),"#",casino_card:card_to_string(T)]).

one_card(Card)->
	casino_card:one_card(Card).