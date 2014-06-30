-module(dragontiger_game_api).
-export([start_game_server/2]).
-export([dealer_connect/2,dealer_disconnect/1]).
-export([player_join/3,player_quit/3]).
-export([new_shoe/1,start_bet/1,stop_bet/1,commit/1]).
-export([try_bet/3,bet_fail/2,bet_succeed/3]).
-export([deal/3,clear/2,scan/2]).
-export([update_countdown/2]).
-export([ace/0,two/0,three/0,four/0,five/0,six/0,seven/0,eight/0,nine/0,ten/0,jack/0,queen/0,king/0]).

-include("user.hrl").
-include("table.hrl").

-define(SERVER,dragontiger_game).

start_game_server(DealerTableId,Countdown) when  is_integer(DealerTableId) andalso is_integer(Countdown)->
	GameServer=global_game_server(DealerTableId),
	gen_fsm:start_link(GameServer,?SERVER,{DealerTableId,Countdown},[]).

global_game_server(DealerTableId)->
	{global,{game_server,DealerTableId}}.

new_shoe(GameServer)->
	gen_fsm:sync_send_event(GameServer,new_shoe).

start_bet(GameServer)->
	gen_fsm:sync_send_event(GameServer,start_bet).

try_bet(GameServer,Cats,Amounts) ->
	gen_fsm:sync_send_event(GameServer,{try_bet,Cats,Amounts}).
bet_succeed(_GameServer,_Tag,_BetBundleId)->
	ok.
bet_fail(_GameServer,_Tag)->
	ok.

stop_bet(GameServer)->
	gen_fsm:sync_send_event(GameServer,stop_bet).

scan(GameServer,Card) when is_record(Card,card) ->
	gen_fsm:sync_send_event(GameServer,{scan,Card}).

deal(GameServer,Pos,Card) when is_integer(Pos) andalso is_record(Card,card) ->
	gen_fsm:sync_send_event(GameServer,{deal,Pos,Card});
		
clear(GameServer,Pos)->
	gen_fsm:sync_send_event(GameServer,{clear,Pos});

commit(GameServer)->
	gen_fsm:sync_send_event(GameServer,commit).

dealer_connect(GameServer,Dealer)->
	gen_fsm:sync_send_all_state_event(GameServer,{dealer_connect,Dealer}).

dealer_disconnect(GameServer)->
	gen_fsm:send_all_state_event(GameServer,{dealer_disconnect,self()}).

update_countdown(GameServer,Countdown)->
	gen_fsm:sync_send_all_state_event(GameServer,{update_countdown,Countdown}).

player_join(GameServer,User,PlayerTable) when is_record(User,user) andalso is_record(PlayerTable,player_table)->
	gen_fsm:sync_send_all_state_event(GameServer,{player_join,User,PlayerTable}).
player_quit(GameServer,User,Reason)->
	gen_fsm:send_all_state_event(GameServer,{player_quit,User,Reason}).


ace()->?ACE#card{suit=?S_CLUB}.
two()->?TWO#card{suit=?S_CLUB}.
three()->?THREE#card{suit=?S_CLUB}.
four()->?FOUR#card{suit=?S_CLUB}.
five()->?FIVE#card{suit=?S_CLUB}.
six()->?SIX#card{suit=?S_CLUB}.
seven()->?SEVEN#card{suit=?S_CLUB}.
eight()->?EIGHT#card{suit=?S_CLUB}.
nine()->?NINE#card{suit=?S_CLUB}.
ten()->?TEN#card{suit=?S_CLUB}.
jack()->?JACK#card{suit=?S_CLUB}.
queen()->?QUEEN#card{suit=?S_CLUB}.
king()->?KING#card{suit=?S_CLUB}.