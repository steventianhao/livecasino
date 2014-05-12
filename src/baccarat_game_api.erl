-module(baccarat_game_api).
-export([start_link/2]).
-export([start_bet/0,stop_bet/0,commit/0,dealer_connect/1,
	dealer_disconnect/1,deal/2,clear/1,update_countdown/1]).
-define(SERVER,baccarat_game).

start_link(Countdown,Table)->
	gen_fsm:start_link({local,?SERVER},?SERVER,{Countdown,Table,undefined},[]).

start_bet()->
	gen_fsm:sync_send_event(?SERVER,start_bet).

stop_bet()->
	gen_fsm:sync_send_event(?SERVER,stop_bet).

deal(Pos,Card)->
	gen_fsm:sync_send_event(?SERVER,{deal,Pos,Card}).

clear(Pos)->
	gen_fsm:sync_send_event(?SERVER,{clear,Pos}).

commit()->
	gen_fsm:sync_send_event(?SERVER,commit).

dealer_connect(Dealer)->
	gen_fsm:sync_send_all_state_event(?SERVER,{dealer_connect,Dealer}).

dealer_disconnect(Pid)->
	gen_fsm:send_all_state_event(?SERVER,{dealer_disconnect,Pid}).

update_countdown(Countdown)->
	gen_fsm:sync_send_all_state_event(?SERVER,{update_countdown,Countdown}).
