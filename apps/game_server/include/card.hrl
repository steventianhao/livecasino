-ifndef (CARD_HRL).
-define(CARD_HRL,true).

-define(S_HEART,$H).
-define(S_SPADE,$S).
-define(S_DIAMOND,$D).
-define(S_CLUB,$C).
-define(ALL_SUIT,[?S_HEART,?S_SPADE,?S_DIAMOND,?S_CLUB]).

-define(R_ACE,$A).
-define(R_TWO,$2).
-define(R_THREE,$3).
-define(R_FOUR,$4).
-define(R_FIVE,$5).
-define(R_SIX,$6).
-define(R_SEVEN,$7).
-define(R_EIGHT,$8).
-define(R_NINE,$9).
-define(R_TEN,$T).
-define(R_JACK,$J).
-define(R_QUEEN,$Q).
-define(R_KING,$K).
-define(ALL_RANK,[?R_ACE,?R_TWO,?R_THREE,?R_FOUR,?R_FIVE,?R_SIX,?R_SEVEN,?R_EIGHT,?R_NINE,?R_TEN,?R_JACK,?R_QUEEN,?R_KING]).

-record(card,{suit,rank,value}).
-define(C_ACE(V),#card{rank=?R_ACE,value=V}).
-define(C_TWO(V),#card{rank=?R_TWO,value=V}).
-define(C_THREE(V),#card{rank=?R_THREE,value=V}).
-define(C_FOUR(V),#card{rank=?R_FOUR,value=V}).
-define(C_FIVE(V),#card{rank=?R_FIVE,value=V}).
-define(C_SIX(V),#card{rank=?R_SIX,value=V}).
-define(C_SEVEN(V),#card{rank=?R_SEVEN,value=V}).
-define(C_EIGHT(V),#card{rank=?R_EIGHT,value=V}).
-define(C_NINE(V),#card{rank=?R_NINE,value=V}).
-define(C_TEN(V),#card{rank=?R_TEN,value=V}).
-define(C_JACK(V),#card{rank=?R_JACK,value=V}).
-define(C_QUEEN(V),#card{rank=?R_QUEEN,value=V}).
-define(C_KING(V),#card{rank=?R_KING,value=V}).

-endif.