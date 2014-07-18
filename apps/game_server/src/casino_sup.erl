-module(casino_sup).
-behavior(supervisor).
-include("game.hrl").
-include("table.hrl").
-export([start_link/0,start_player/6]).
-export([init/1]).

start_link()->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

start_player(GameName,DealerTableId,Server,PlayerTable,User,UserPid)->
	Id=players_sup_id(GameName,DealerTableId),
	supervisor:start_child(Id,[Server,PlayerTable,User,UserPid]).

game_server_id(GameName,Table)->
	unique_id(GameName,"_game_",Table).

players_sup_id(GameName,Table)->
	unique_id(GameName,"_players_sup_",Table).

unique_id(GameName,Middle,Table)->
	list_to_atom(lists:concat([GameName,Middle,Table])).

players_sup_spec(Game,Table)->
	Id=players_sup_id(Game#game.name,Table),
	StartFunc={casino_players_sup,start_link,[Id,Game,Table]},
	{Id,StartFunc,transient,6,supervisor,dynamic}.

game_server_spec(Game,Table,Countdown,PlayerTables)->
	StartFunc={game_server_one,start_game_server,[Game,Table,Countdown,PlayerTables]},
	Id=game_server_id(Game#game.name,Table),
	{Id,StartFunc,permanent,6,worker,[game_server_one]}.

dragontiger(Table,Countdown)->
	[game_server_spec(#game{name=dragontiger,module=dragontiger_game_mod},Table,Countdown,[?PLAYER_TAB4]),
	players_sup_spec(#game{name=dragontiger,module=dragontiger_player_mod},Table)].

baccarat(Table,Countdown)->
	[game_server_spec(#game{name=baccarat,module=baccarat_game_mod},Table,Countdown,[?PLAYER_TAB1,?PLAYER_TAB101]),
	players_sup_spec(#game{name=baccarat,module=baccarat_player_mod},Table)].

init([])->
	Children=baccarat(1,15)++dragontiger(4,13),
	RestartStrategy={one_for_one,1,60},
	{ok,{RestartStrategy,Children}}.