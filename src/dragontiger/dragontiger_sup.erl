-module(dragontiger_sup).
-behavior(supervisor).

-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link()->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

dragontiger_spec()->
	StartFunc={dragontiger_game_api,start_game_server,[4,13]},
	{dragontiger_game,StartFunc,permanent,6,worker,[dragontiger_game]}.

players_sup_spec(Table)->
	StartFunc={dragontiger_players_sup,start_link,[]},
	Id=list_to_atom(lists:concat(["dragontiger_players_sup_",Table])),
	{Id,StartFunc,transient,6,supervisor,dynamic}.

init([])->
	Children=[dragontiger_spec()],
	RestartStrategy={one_for_one,1,60},
	{ok,{RestartStrategy,Children}}.