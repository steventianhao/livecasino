-module(dealer_game_api).

-export([find_server/1,connect/3,disconnect/1,new_shoe/1,start_bet/1,stop_bet/1]).
-export([scan/3,deal/4,clear/3,commit/1,update_countdown/2]).

find_server(Table)->
	global:whereis_name({game_server,Table}).

new_shoe(GameServer)->
	gen_fsm:sync_send_event(GameServer,new_shoe).

start_bet(GameServer)->
	gen_fsm:sync_send_event(GameServer,start_bet).

stop_bet(GameServer)->
	gen_fsm:sync_send_event(GameServer,stop_bet).

scan(GameServer,Card)->
	case dealer_cards:check_scan(Card) of
		true ->
			gen_fsm:sync_send_event(GameServer,{scan,Card});
		false ->
			error
	end.

deal(GameServer,Game,Pos,Card)->
	case dealer_cards:check_deal(Game,Pos,Card) of
		true ->
			gen_fsm:sync_send_event(GameServer,{deal,Pos,Card});
		false->
			error
	end.
		
clear(GameServer,Game,Pos)->
	case dealer_cards:check_clear(Game,Pos) of
		true ->
			gen_fsm:sync_send_event(GameServer,{clear,Pos});
		false->
			error
	end.

commit(GameServer)->
	gen_fsm:sync_send_event(GameServer,commit).

connect(GameServer,DealerId,DealerName)->
	gen_fsm:sync_send_all_state_event(GameServer,{dealer_connect,{dealer,DealerId,DealerName}}).

disconnect(GameServer)->
	gen_fsm:send_all_state_event(GameServer,{dealer_disconnect,self()}).

update_countdown(GameServer,Countdown)->
	gen_fsm:sync_send_all_state_event(GameServer,{update_countdown,Countdown}).
