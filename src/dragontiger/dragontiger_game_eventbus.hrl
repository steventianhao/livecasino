-ifndef(DRAGONTIGER_GAME_EVENTBUS_HRL).
-define(DRAGONTIGER_GAME_EVENTBUS_HRL,true).



-define(GAME_SERVER_EVENT_BUS(DealerTableId),{global,{game_eventbus,dragontiger,DealerTableId}}).
-define(GAME_PLAYER_HANDLER,dragontiger_player_handler).

-define(START_EVENTBUS(DealerTableId),gen_event:start_link(?GAME_SERVER_EVENT_BUS(DealerTableId))).

-define(NOTIFY(DealerTableId,Event),gen_event:notify(?GAME_SERVER_EVENT_BUS(DealerTableId),Event)).

-define(ADD_HANDLER(DealerTableId,PlayerId,Args),gen_event:add_handler(?GAME_SERVER_EVENT_BUS(DealerTableId),{?GAME_PLAYER_HANDLER,PlayerId},Args)).
-define(DELETE_HANDLER(DealerTableId,PlayerId,Args),gen_event:delete_handler(?GAME_SERVER_EVENT_BUS(DealerTableId),{?GAME_PLAYER_HANDLER,PlayerId},Args)).

-endif.