-module(player_baccarat).
-export([check_bets/2]).

-define(BET_BANKER,1000).
-define(BET_PLAYER,1001).
-define(BET_TIE,1002).
-define(BET_BANKER_PAIR,1003).
-define(BET_PLAYER_PAIR,1004).
-define(BET_BANKER_N8,1005).
-define(BET_BANKER_N9,1006).
-define(BET_PLAYER_N8,1007).
-define(BET_PLAYER_N9,1008).
-define(BET_BIG,1009).
-define(BET_SMALL,1010).

-define(ALL_BET_CATS,[?BET_BANKER,?BET_PLAYER,?BET_TIE,
	?BET_BANKER_PAIR,?BET_PLAYER_PAIR,
	?BET_BANKER_N8,?BET_BANKER_N9,?BET_PLAYER_N8,?BET_PLAYER_N9,
	?BET_BIG,?BET_SMALL]).

check_bets(Cats,Amounts)->
	player_cards:is_valid_bets(Cats,Amounts,?ALL_BET_CATS).