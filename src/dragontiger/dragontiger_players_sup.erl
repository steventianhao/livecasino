-module(dragontiger_players_sup).
-behavior(supervisor).

-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

start_link(Table)->
	Name=dragontiger_sup:players_sup_id(Table),
	supervisor:start_link({local,Name},?MODULE,[]).

init([])->
	PlayerSpec={player,{dragontiger_player,start_link,[]},transient,6,worker,[dragontiger_player]},
	RestartStrategy={simple_one_for_one,1,60},
	{ok,{RestartStrategy,[PlayerSpec]}}.