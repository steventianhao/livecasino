-module(dragontiger_sup).
-behavior(supervisor).

-export([start_link/0,players_sup_id/1]).

%% supervisor callbacks
-export([init/1]).

start_link()->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

dragontiger_spec(Table,Countdown)->
	StartFunc={dragontiger_game_api,start_game_server,[Table,Countdown]},
	Id=list_to_atom(lists:concat(["dragontiger_game_",Table])),
	{Id,StartFunc,permanent,6,worker,[dragontiger_game]}.

players_sup_id(Table)->
	list_to_atom(lists:concat(["dragontiger_players_sup_",Table])).

players_sup_spec(Table)->
	StartFunc={dragontiger_players_sup,start_link,[Table]},
	Id=players_sup_id(Table),
	{Id,StartFunc,transient,6,supervisor,dynamic}.

init([])->
	Children=[dragontiger_spec(4,13),players_sup_spec(4)],
	RestartStrategy={one_for_one,1,60},
	{ok,{RestartStrategy,Children}}.