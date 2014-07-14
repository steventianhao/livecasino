-module(dealer_dragontiger).
-export([check_pos/1]).

-define(DRAGON_POS,1).
-define(TIGER_POS,2).
-define(ALL_POS,[?DRAGON_POS,?TIGER_POS]).

check_pos(Pos)->
	lists:member(Pos,?ALL_POS).
