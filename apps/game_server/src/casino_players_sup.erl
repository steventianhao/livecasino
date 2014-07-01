-module(casino_players_sup).
-behavior(supervisor).

-export([start_link/3]).

%% supervisor callbacks
-export([init/1]).

start_link(Name,Table,PlayerMod)->
	supervisor:start_link({local,Name},?MODULE,PlayerMod).

init(PlayerMod)->
	PlayerSpec={player,{PlayerMod,start_link,[]},transient,6,worker,[PlayerMod]},
	RestartStrategy={simple_one_for_one,1,60},
	{ok,{RestartStrategy,[PlayerSpec]}}.