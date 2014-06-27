-module(casino_players_sup).
-behavior(supervisor).

-export([start_link/2]).

%% supervisor callbacks
-export([init/1]).

start_link(Table,PlayerMod)->
	Name=casino_sup:players_sup_id(Table),
	supervisor:start_link({local,Name},?MODULE,PlayerMod).

init(PlayerMod)->
	PlayerSpec={player,{PlayerMod,start_link,[]},transient,6,worker,[PlayerMod]},
	RestartStrategy={simple_one_for_one,1,60},
	{ok,{RestartStrategy,[PlayerSpec]}}.