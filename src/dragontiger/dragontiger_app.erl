-module(dragontiger_app).

-behaviour(application).

-export([start/2,stop/1]).

start(_Type,_StartArgs)->
	mysql_casino_master:add_db_pool(),
	dragontiger_sup:start_link().

stop(_State)->
	ok.