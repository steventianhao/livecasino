-module(casino_players_sup).
-behavior(supervisor).

-export([start_link/2]).

%% supervisor callbacks
-export([init/1]).

start_link(Name,Game)->
	supervisor:start_link({local,Name},?MODULE,[Game]).

init([Game])->
	PlayerSpec={player,{game_player_one,start_link,[Game]},transient,6,worker,[game_player_one]},
	RestartStrategy={simple_one_for_one,1,60},
	{ok,{RestartStrategy,[PlayerSpec]}}.