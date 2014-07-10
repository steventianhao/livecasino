-module(game_api).

-export([find_server/1,connect/3,disconnect/1,new_shoe/1,start_bet/1,stop_bet/1]).
-export([scan/2,deal/3,clear/2,commit/1,update_countdown/2]).


-define(ALL_SUIT,[$H,$S,$D,$C]).
-define(ALL_RANK,[$A,$2,$3,$4,$5,$6,$7,$8,$9,$T,$J,$Q,$K]).

check_one_card(S,R) when is_integer(S) andalso is_integer(R)->
	lists:member(S,?ALL_SUIT) andalso lists:member(R,?ALL_RANK);
check_one_card(_,_)->
	false.

find_server(Table)->
	global:whereis_name({game_server,Table}).

new_shoe(GameServer)->
	gen_fsm:sync_send_event(GameServer,new_shoe).

start_bet(GameServer)->
	gen_fsm:sync_send_event(GameServer,start_bet).

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

connect(GameServer,DealerId,DealerName)->
	gen_fsm:sync_send_all_state_event(GameServer,{dealer_connect,DealerId,DealerName}).

disconnect(GameServer)->
	gen_fsm:send_all_state_event(GameServer,{dealer_disconnect,self()}).

update_countdown(GameServer,Countdown)->
	gen_fsm:sync_send_all_state_event(GameServer,{update_countdown,Countdown}).
