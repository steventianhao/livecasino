-module(player_ws_handler).
-behaviour(cowboy_websocket_handler).
-include("handler_state.hrl").

-export([init/3,websocket_init/3,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

init({tcp,http},_Req,_Otps)->
	{upgrade,protocol,cowboy_websocket}.

websocket_init(_TransportName,Req,_Otps)->
	self() ! auth,
	{ok,Req,#state{}}.

websocket_handle({text,Msg},Req,State)->
	{Json,NewState}=player_action_handler:handle_msg(Msg,State),
	{reply,{text,Json},Req,NewState};
	
websocket_handle(_Data,Req,State)->
	{ok,Req,State}.

websocket_info(auth,Req,State)->
	{reply,{text,player_action_handler:auth_json()},Req,State};

websocket_info({json,Json},Req,State)->
	{reply,{text,Json},Req,State};

websocket_info(Info,Req,State)->
	io:format("info is ~p~n",[Info]),
	{ok,Req,State}.

websocket_terminate(Reason,_Req,_State)->
	io:format("terminated reason is ~p~n",[Reason]),
	ok.