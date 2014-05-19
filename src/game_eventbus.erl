-module(game_eventbus).

-export([global_game_eventbus/1,start_game_eventbus/1,notify/2,add_player_handler/4,del_player_handler/4]).

global_game_eventbus(DealerTableId)->
	{global,{game_eventbus,DealerTableId}}.

start_game_eventbus(EventBus)->
	gen_event:start_link(EventBus).

notify(EventBus,Event)->
	gen_event:notify(EventBus,Event).

add_player_handler(EventBus,Module,PlayerId,Args)->
	gen_event:add_handler(EventBus,{Module,PlayerId},Args).

del_player_handler(EventBus,Module,PlayerId,Args)->
	gen_event:delete_handler(EventBus,{Module,PlayerId},Args).