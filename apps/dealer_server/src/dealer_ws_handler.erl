-module(dealer_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3,websocket_init/3,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

init({tcp,http},_Req,_Otps)->
	{upgrade,protocol,cowboy_websocket}.

websocket_init(_TransportName,Req,_Otps)->
	self()! auth,
	{ok,Req,undefined}.

websocket_handle({text,Msg},Req,State)->
	{_,Json}=handle_msg(Msg),
	{reply,{text,Json},Req,State};
websocket_handle(_Data,Req,State)->
	{ok,Req,State}.

websocket_info(auth,Req,State)->
	Json=jsx:encode([{<<"kind">>,<<"auth">>}]),
	{reply,{text,Json},Req,State};
websocket_info(Info,Req,State)->
	io:format("info is ~p~n",[Info]),
	{ok,Req,State}.

websocket_terminate(Reason,_Req,_State)->
	io:format("terminated reason is ~p~n",[Reason]),
	ok.

handle_msg(Msg)->
	io:format("this is msg we got ~p~n",[Msg]),
	case jsx:is_json(Msg) of
		true-> 
			{ok,handle_json(Msg)};
		false->
			{error,jsx:encode([{<<"kind">>,<<"invalid">>}])}
	end.

handle_json(Msg)->
	Msg.