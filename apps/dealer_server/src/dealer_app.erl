-module(dealer_app).
-behavior(application).
-export([start/2,stop/1]).

-define(APP,dealer).

start(_StartType,_StartArgs)->
	{ok,Port}=application:get_env(?APP,listen_port),
	Routes=[{'_',[
		{"/",cowboy_static,{priv_file,?APP,"index.html"}},
		{"/websocket",dealer_ws_handler,[]},
		{"/static/[...]",{priv_dir,?APP,"static"}}
	]}],
	Dispatch=cowboy_router:compile(Routes),
	{ok,_}=cowboy:start_http(dealer_http,100,[{port,Port}],
		[{env,[{dispatch,Dispatch}]}]),
	dealer_sup:start_link().

stop(_State)->
	cowboy:stop_listener(dealer_http),
	ok.