-module(dragontiger_game_mod).
-export([put/3,add/2,remove/2,validate/1,to_string/1]).

-define(GAME_DEALER_MOD,dragontiger_dealer_mod).

put(Pos,Card,Cards)->
	?GAME_DEALER_MOD:put(Pos,Card,Cards).

add(Card,Cards)->
	?GAME_DEALER_MOD:add(Card,Cards).

remove(Pos,Cards)->
	?GAME_DEALER_MOD:remove(Pos,Cards).

validate(Cards)->
	?GAME_DEALER_MOD:validate(Cards).

to_string(Cards)->
	?GAME_DEALER_MOD:to_string(Cards).