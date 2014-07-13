-module(player_dragontiger).
-export([check_bets/2]).

-define(BET_DRAGON,2000).
-define(BET_TIGER,2001).
-define(BET_TIE,2002).
-define(BET_DRAGON_ODD,2003).
-define(BET_TIGER_ODD,2004).
-define(BET_DRAGON_EVEN,2005).
-define(BET_TIGER_EVEN,2006).

-define(ALL_BET_CATS,[?BET_DRAGON,?BET_TIGER,?BET_TIE,?BET_DRAGON_ODD,?BET_TIGER_ODD,?BET_DRAGON_EVEN,?BET_TIGER_EVEN]).

check_bets(Cats,Amounts)->
	player_cards:is_valid_bets(Cats,Amounts,?ALL_BET_CATS).