-module(dragontiger_players_sup).
-behavior(supervisor).

-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link()->
	supervisor:start_link(?MODULE,[]).

init([])->
	PlayerSpec={player,{dragontiger_player,init,[]},transient,6,worker,[dragontiger_player]},
	RestartStrategy={simple_one_for_one,1,60},
	{ok,{RestartStrategy,[PlayerSpec]}}.