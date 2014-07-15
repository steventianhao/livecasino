-module(casino_sup).
-behavior(supervisor).
-include("game.hrl").

-export([start_link/0,start_player/6]).
%% supervisor callbacks
-export([init/1]).

start_link()->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

start_player(baccarat,DealerTableId,Server,EventBus,PlayerTableId,User)->
	supervisor:start_child(baccarat_players_sup_id(DealerTableId),[Server,EventBus,PlayerTableId,User]);
start_player(dragontiger,DealerTableId,Server,EventBus,PlayerTableId,User)->
	supervisor:start_child(dragontiger_players_sup_id(DealerTableId),[Server,EventBus,PlayerTableId,User]).

child_id(Prefix,Table)->
	list_to_atom(lists:concat([Prefix,Table])).

dragontiger_players_sup_id(Table)->
	child_id("dragontiger_players_sup_",Table).

baccarat_players_sup_id(Table)->
	child_id("baccarat_players_sup_",Table).

dragontiger_spec(Table,Countdown)->
	Game=#game{name=dragontiger,module=dragontiger_game_mod},
	StartFunc={game_server_one,start_game_server,[Game,Table,Countdown]},
	Id=child_id("dragontiger_game_",Table),
	{Id,StartFunc,permanent,6,worker,[dragontiger_game]}.

dragontiger_players_sup_spec(Table)->
	Id=dragontiger_players_sup_id(Table),
	StartFunc={casino_players_sup,start_link,[Id,Table,dragontiger_player]},
	{Id,StartFunc,transient,6,supervisor,dynamic}.

baccarat_spec(Table,Countdown)->
	Game=#game{name=baccarat,module=baccarat_game_mod},
	StartFunc={game_server_one,start_game_server,[Game,Table,Countdown]},
	Id=child_id("baccarat_game_",Table),
	{Id,StartFunc,permanent,6,worker,[baccarat_game]}.

baccarat_players_sup_spec(Table)->
	Id=baccarat_players_sup_id(Table),
	StartFunc={casino_players_sup,start_link,[Id,Table,baccarat_player]},
	{Id,StartFunc,transient,6,supervisor,dynamic}.	

dragontiger(Table,Countdown)->
	[dragontiger_spec(Table,Countdown),dragontiger_players_sup_spec(Table)].

baccarat(Table,Countdown)->
	[baccarat_spec(Table,Countdown),baccarat_players_sup_spec(Table)].

init([])->
	Children=baccarat(1,15)++dragontiger(4,13),
	RestartStrategy={one_for_one,1,60},
	{ok,{RestartStrategy,Children}}.