-ifndef(BACCARAT_HRL).
-define(BACCARAT_HRL,true).

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
-define(TEN,?C_TEN(0)).
-define(JACK,?C_JACK(0)).
-define(QUEEN,?C_QUEEN(0)).
-define(KING,?C_KING(0)).

-define(ALL_CARD,[?ACE,?TWO,?THREE,?FOUR,?FIVE,?SIX,?SEVEN,?EIGHT,?NINE,?TEN,?JACK,?QUEEN,?KING]).

-define(BANKER_POS_1,2).
-define(BANKER_POS_2,4).
-define(BANKER_POS_3,6).
-define(PLAYER_POS_1,1).
-define(PLAYER_POS_2,3).
-define(PLAYER_POS_3,5).

-define(TOTAL89,[8,9]).
-define(TOTAL67,[6,7]).
-define(ALL_POS,[?PLAYER_POS_1,?BANKER_POS_1,?PLAYER_POS_2,?BANKER_POS_2,?PLAYER_POS_3,?BANKER_POS_3]).

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
-endif.