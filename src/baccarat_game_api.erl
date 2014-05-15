-module(baccarat_game_api).
-export([start_link/2,start_eventbus/0]).
-export([dealer_connect/1,dealer_disconnect/1]).
-export([new_shoe/0,start_bet/0,stop_bet/0,commit/0,bet/2]).
-export([deal/2,clear/1,scan/1]).
-export([update_countdown/1]).
-export([ace/0,two/0,three/0,four/0,five/0,six/0,seven/0,eight/0,nine/0,ten/0,jack/0,queen/0,king/0]).


-include("baccarat_game_eventbus.hrl").

-define(SERVER,baccarat_game).

start_eventbus()-> ?START_EVENTBUS.

start_link(Countdown,Table)->
	gen_fsm:start_link({local,?SERVER},?SERVER,{Countdown,Table},[]).

new_shoe()->
	gen_fsm:sync_send_event(?SERVER,new_shoe).

start_bet()->
	gen_fsm:sync_send_event(?SERVER,start_bet).

bet(Cats,Amounts)->
	gen_fsm:sync_send_event(?SERVER,{bet,Cats,Amounts}).

stop_bet()->
	gen_fsm:sync_send_event(?SERVER,stop_bet).

scan(Card)->
	gen_fsm:sync_send_event(?SERVER,{scan,Card}).

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


-include("baccarat.hrl").
ace()->?ACE.
two()->?TWO.
three()->?THREE.
four()->?FOUR.
five()->?FIVE.
six()->?SIX.
seven()->?SEVEN.
eight()->?EIGHT.
nine()->?NINE.
ten()->?TEN.
jack()->?JACK.
queen()->?QUEEN.
king()->?KING.