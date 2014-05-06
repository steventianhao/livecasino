-module(baccarat_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link()->
	supervisor:start_link(?MODULE,[]).

init([])->
	Children=[],
	RestartStrategy={one_for_one,7,10},
	{ok,{RestartStrategy,Children}}.