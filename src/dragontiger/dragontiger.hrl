-ifndef(DRAGONTIGER_HRL).
-define(DRAGONTIGER_HRL,true).

-record(card,{name,suit,value}).

-define(CARD(N,V),#card{name=N,value=V}).

-define(ACE,?CARD($A,1)).
-define(TWO,?CARD($2,2)).
-define(THREE,?CARD($3,3)).
-define(FOUR,?CARD($4,4)).
-define(FIVE,?CARD($5,5)).
-define(SIX,?CARD($6,6)).
-define(SEVEN,?CARD($7,7)).
-define(EIGHT,?CARD($8,8)).
-define(NINE,?CARD($9,9)).
-define(TEN,?CARD($T,10)).
-define(JACK,?CARD($J,11)).
-define(QUEEN,?CARD($Q,12)).
-define(KING,?CARD($K,13)).

-define(ALL_CARD,[?ACE,?TWO,?THREE,?FOUR,?FIVE,?SIX,?SEVEN,?EIGHT,?NINE,?TEN,?JACK,?QUEEN,?KING]).
-define(CARDS_MAP,maps:from_list([{C#card.name,C} || C <-?ALL_CARD])).

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