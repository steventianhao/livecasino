-ifndef(BACCARAT_HRL).
-define(BACCARAT_HRL,true).

-record(card,{name,value}).

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
-define(TEN,?CARD($T,0)).
-define(JACK,?CARD($J,0)).
-define(QUEEN,?CARD($Q,0)).
-define(KING,?CARD($K,0)).

-define(ALL_CARD,[?ACE,?TWO,?THREE,?FOUR,?FIVE,?SIX,?SEVEN,?EIGHT,?NINE,?TEN,?JACK,?QUEEN,?KING]).
-define(CARDS_MAP,maps:from_list([{C#card.name,C} || C <-?ALL_CARD])).

-define(INVALID_POS,-1).
-define(BANKER_POS_1,2).
-define(BANKER_POS_2,4).
-define(BANKER_POS_3,6).
-define(PLAYER_POS_1,1).
-define(PLAYER_POS_2,3).
-define(PLAYER_POS_3,5).

-define(TOTAL89,[8,9]).
-define(TOTAL67,[6,7]).
-define(ALL_POS,[?PLAYER_POS_1,?BANKER_POS_1,?PLAYER_POS_2,?BANKER_POS_2,?PLAYER_POS_3,?BANKER_POS_3]).

-endif.