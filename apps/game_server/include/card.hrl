-ifndef (CARD_HRL).
-define(CARD_HRL,true).

-define(HEART,$H).
-define(SPADE,$S).
-define(DIAMOND,$D).
-define(CLUB,$C).
-define(ALL_SUIT,[?HEART,?SPADE,?DIAMOND,?CLUB]).

-define(ACE,$A).
-define(TWO,$2).
-define(THREE,$3).
-define(FOUR,$4).
-define(FIVE,$5).
-define(SIX,$6).
-define(SEVEN,$7).
-define(EIGHT,$8).
-define(NINE,$9).
-define(TEN,$T).
-define(JACK,$J).
-define(QUEEN,$Q).
-define(KING,$K).
-define(ALL_RANK,[?ACE,?TWO,?THREE,?FOUR,?FIVE,?SIX,?SEVEN,?EIGHT,?NINE,?TEN,?JACK,?QUEEN,?KING]).

-record(card,{suit,rank}).
-endif.