-module(dealer_baccarat).
-export([check_pos/1]).

-define(BANKER_POS_1,2).
-define(BANKER_POS_2,4).
-define(BANKER_POS_3,6).
-define(PLAYER_POS_1,1).
-define(PLAYER_POS_2,3).
-define(PLAYER_POS_3,5).
-define(ALL_POS,[?PLAYER_POS_1,?BANKER_POS_1,?PLAYER_POS_2,?BANKER_POS_2,?PLAYER_POS_3,?BANKER_POS_3]).

check_pos(Pos)->
	lists:member(Pos,?ALL_POS).