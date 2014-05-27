-ifndef(ROUND_HRL).
-define(ROUND_HRL,true).

-define(IDLE,0).
-define(BETTING,1).
-define(DEALING,2).
-define(DONE,3).

-record(round,{id,shoeIndex,roundIndex,cards,createTime}).

-endif.