-module(game_api).

-export([dealer_connect/2,dealer_disconnect/1]).
-export([new_shoe/1,start_bet/1,stop_bet/1,commit/1,try_bet/3]).
-export([deal/3,clear/2,scan/2]).
-export([update_countdown/2]).
-export([player_join/3,player_quit/3]).

-include("user.hrl").
-include("table.hrl").
	
new_shoe(GameServer)->
	gen_fsm:sync_send_event(GameServer,new_shoe).

start_bet(GameServer)->
	gen_fsm:sync_send_event(GameServer,start_bet).

try_bet(GameServer,Cats,Amounts)->
	gen_fsm:sync_send_event(GameServer,{try_bet,Cats,Amounts}).

stop_bet(GameServer)->
	gen_fsm:sync_send_event(GameServer,stop_bet).

scan(GameServer,Card)->
	gen_fsm:sync_send_event(GameServer,{scan,Card}).

deal(GameServer,Pos,Card)->
	gen_fsm:sync_send_event(GameServer,{deal,Pos,Card}).

clear(GameServer,Pos)->
	gen_fsm:sync_send_event(GameServer,{clear,Pos}).

commit(GameServer)->
	gen_fsm:sync_send_event(GameServer,commit).

dealer_connect(GameServer,Dealer)->
	gen_fsm:sync_send_all_state_event(GameServer,{dealer_connect,Dealer}).

dealer_disconnect(GameServer)->
	gen_fsm:send_all_state_event(GameServer,{dealer_disconnect,self()}).

update_countdown(GameServer,Countdown)->
	gen_fsm:sync_send_all_state_event(GameServer,{update_countdown,Countdown}).

player_join(GameServer,User,PlayerTableId) when is_record(User,user) andalso is_integer(PlayerTableId)->
	gen_fsm:sync_send_all_state_event(GameServer,{player_join,User,PlayerTableId}).
player_quit(GameServer,User,Reason)->
	gen_fsm:send_all_state_event(GameServer,{player_quit,User,Reason}).