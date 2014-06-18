-module(dragontiger_sup).
-behavior(supervisor).

-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link()->
	supervisor:start_link(?MODULE,[]).

dragontiger_spec()->
	StartFunc={dragontiger_game_api,start_game_server,[4,13]},
	{dragontiger_game,StartFunc,permanent,6,worker,[dragontiger_game]}.

players_supervisor()->
	StartFunc={dragontiger_players_sup,start_link,[]},
	{dragontiger_players_sup,StartFunc,transient,6,supervisor,dynamic}.

init([])->
	Children=[dragontiger_spec(),players_supervisor()],
	RestartStrategy={one_for_one,1,60},
	{ok,{RestartStrategy,Children}}.