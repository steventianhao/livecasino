-ifndef (TABLE_HRL).
-define(TABLE_HRL,true).

-record(player_table,{id,payout}).
-record(dealer_table,{id,game,countdown}).

-record(table,{index,dealer_table,player_tables}).

-define(DEALER_TAB1,#dealer_table{id=1,game=baccarat,countdown=15}).
-define(DEALER_TAB2,#dealer_table{id=2,game=baccarat,countdown=15}).
-define(DEALER_TAB3,#dealer_table{id=3,game=baccarat,countdown=15}).
-define(DEALER_TAB4,#dealer_table{id=4,game=dragontiger,countdown=15}).

-define(PLAYER_TAB1,#player_table{id=1,payout=commission}).
-define(PLAYER_TAB101,#player_table{id=101,payout=nocommission}).

-define(PLAYER_TAB2,#player_table{id=2,payout=commission}).
-define(PLAYER_TAB102,#player_table{id=102,payout=nocommission}).

-define(PLAYER_TAB3,#player_table{id=3,payout=commission}).
-define(PLAYER_TAB103,#player_table{id=103,payout=nocommission}).

-define(PLAYER_TAB4,#player_table{id=4,payout=dragontiger}).


-define(TAB1,#table{1,?DEALER_TAB1,[?PLAYER_TAB101,?PLAYER_TAB1]}).
-define(TAB2,#table{2,?DEALER_TAB2,[?PLAYER_TAB102,?PLAYER_TAB2]}).
-define(TAB3,#table{3,?DEALER_TAB3,[?PLAYER_TAB103,?PLAYER_TAB3]}).
-define(TAB4,#table{4,?DEALER_TAB4,[?PLAYER_TAB4]}).

-define(ALL_DEALER_TABLES,[?DEALER_TAB1,?DEALER_TAB2,?DEALER_TAB3,?DEALER_TAB4]).
-define(ALL_TABLES,[?TAB1,?TAB2,?TAB3,?TAB4]).

-endif.