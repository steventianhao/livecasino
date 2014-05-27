-module(dragontiger_player_mod).
-export([is_valid_bets/2]).


-define(BET_CAT_DRAGON,2000).
-define(BET_CAT_TIGER,2001).
-define(BET_CAT_TIE,2002).
-define(BET_CAT_DRAGON_ODD,2003).
-define(BET_CAT_TIGER_ODD,2004).
-define(BET_CAT_DRAGON_EVEN,2005).
-define(BET_CAT_TIGER_EVEN,2006).

-define(BET_CATS_MAP,#{
	?BET_CAT_DRAGON => dragon,
	?BET_CAT_TIGER => tiger,
	?BET_CAT_TIE => tie,
	?BET_CAT_DRAGON_ODD => dragon_odd,
	?BET_CAT_TIGER_ODD => tiger_odd,
	?BET_CAT_DRAGON_EVEN => dragon_even,
	?BET_CAT_TIGER_EVEN => tiger_even
	}).

-define(ALL_BET_CATS,[?BET_CAT_DRAGON,?BET_CAT_TIGER,?BET_CAT_TIE,?BET_CAT_DRAGON_ODD,?BET_CAT_TIGER_ODD,?BET_CAT_DRAGON_EVEN,?BET_CAT_TIGER_EVEN]).

all_bet_cats()->
	sets:from_list(?ALL_BET_CATS).

is_valid_bets(Cats,Amounts)->
	casino_bets:is_valid_bets(Cats,Amounts,all_bet_cats()).