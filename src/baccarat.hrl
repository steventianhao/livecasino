-record(card,{name,value}).

-define(CARD(N,V),#card{name=N,value=V}).

-define(ACE,?CARD('A',1)).
-define(TWO,?CARD('2',2)).
-define(THREE,?CARD('3',3)).
-define(FOUR,?CARD('4',4)).
-define(FIVE,?CARD('5',5)).
-define(SIX,?CARD('6',6)).
-define(SEVEN,?CARD('7',7)).
-define(EIGHT,?CARD('8',8)).
-define(NINE,?CARD('9',9)).
-define(TEN,?CARD('T',0)).
-define(JACK,?CARD('J',0)).
-define(QUEEN,?CARD('Q',0)).
-define(KING,?CARD('K',0)).

-define(INVALID_POS,-1).
-define(BANKER_POS_1,1).
-define(BANKER_POS_2,2).
-define(BANKER_POS_3,3).
-define(PLAYER_POS_1,4).
-define(PLAYER_POS_2,5).
-define(PLAYER_POS_3,6).

-define(TOTAL89,[8,9]).
-define(TOTAL67,[6,7]).
-define(ALL_POS,[?BANKER_POS_1,?BANKER_POS_2,?BANKER_POS_3,?PLAYER_POS_1,?PLAYER_POS_2,?PLAYER_POS_3]).