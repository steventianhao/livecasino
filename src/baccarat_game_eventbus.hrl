-ifndef(BACCARAT_GAME_EVENTBUS_HRL).
-define(BACCARAT_GAME_EVENTBUS_HRL,true).

-define(GAME_SERVER_EVENT_BUS,baccarat_game_eventbus).
-define(GAME_PLAYER_HANDLER,baccarat_player_handler).
-define(START_EVENTBUS,gen_event:start_link({local,?GAME_SERVER_EVENT_BUS})).
-define(NOTIFY(Event),gen_event:notify(?GAME_SERVER_EVENT_BUS,Event)).
-define(ADD_HANDLER(Args),gen_event:add_handler(?GAME_SERVER_EVENT_BUS,?GAME_PLAYER_HANDLER,Args)).
-define(DELETE_HANDLER(Args),gen_event:delete_handler(?GAME_SERVER_EVENT_BUS,?GAME_PLAYER_HANDLER,Args)).

-endif.