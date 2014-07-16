-module(casino_players_sup).
-behavior(supervisor).

-export([start_link/3]).

%% supervisor callbacks
-export([init/1]).

start_link(Id,Game,Table)->
	supervisor:start_link({local,Id},?MODULE,[Game,Table]).

init([Game,Table])->
	PlayerSpec={player,{game_player_one,start_link,[Game,Table]},transient,6,worker,[game_player_one]},
	RestartStrategy={simple_one_for_one,1,60},
	{ok,{RestartStrategy,[PlayerSpec]}}.