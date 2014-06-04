-ifndef(DRAGONTIGER_HRL).
-define(DRAGONTIGER_HRL,true).

-include("card.hrl").

-define(ACE,?C_ACE(1)).
-define(TWO,?C_TWO(2)).
-define(THREE,?C_THREE(3)).
-define(FOUR,?C_FOUR(4)).
-define(FIVE,?C_FIVE(5)).
-define(SIX,?C_SIX(6)).
-define(SEVEN,?C_SEVEN(7)).
-define(EIGHT,?C_EIGHT(8)).
-define(NINE,?C_NINE(9)).
-define(TEN,?C_TEN(10)).
-define(JACK,?C_JACK(11)).
-define(QUEEN,?C_QUEEN(12)).
-define(KING,?C_KING(13)).

-define(ALL_CARD,[?ACE,?TWO,?THREE,?FOUR,?FIVE,?SIX,?SEVEN,?EIGHT,?NINE,?TEN,?JACK,?QUEEN,?KING]).
-define(CARDS_MAP,maps:from_list([{C#card.rank,C} || C <-?ALL_CARD])).

-define(DRAGON_POS,1).
-define(TIGER_POS,2).
-define(ALL_POS,[?DRAGON_POS,?TIGER_POS]).

-define(BET_DRAGON,2000).
-define(BET_TIGER,2001).
-define(BET_TIE,2002).
-define(BET_DRAGON_ODD,2003).
-define(BET_TIGER_ODD,2004).
-define(BET_DRAGON_EVEN,2005).
-define(BET_TIGER_EVEN,2006).

-define(ALL_BET_CATS,[?BET_DRAGON,?BET_TIGER,?BET_TIE,?BET_DRAGON_ODD,?BET_TIGER_ODD,?BET_DRAGON_EVEN,?BET_TIGER_EVEN]).

-endif.