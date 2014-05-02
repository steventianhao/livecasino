-module(lobby_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok,_}=ranch:start_listener(listener_name,100,ranch_tcp,[{port,5555}],echo_protocol,[]).
	

stop(_State) ->
    ok.
