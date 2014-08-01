-module(dealer_ws_handler).
-behaviour(cowboy_websocket_handler).
-import(dealer_json,[err_json/1,err_json/2]).

-include("dealer_json.hrl").
-include("dealer_handler.hrl").

-export([init/3,websocket_init/3,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

init({tcp,http},_Req,_Otps)->
	{upgrade,protocol,cowboy_websocket}.

websocket_init(_TransportName,Req,_Otps)->
	self()! auth,
	{ok,Req,#state{}}.

websocket_handle({text,Msg},Req,State)->
	io:format("this is msg we got ~p~n",[Msg]),
	{Json,NewState}=dealer_action_handler:handle_msg(Msg,State),
	{reply,{text,Json},Req,NewState};
websocket_handle(_Data,Req,State)->
	{ok,Req,State}.

websocket_info(auth,Req,State)->
	{reply,{text,err_json(?AUTH)},Req,State};
websocket_info({'DOWN',_Ref,process,Pid,_},Req,#state{table=#table{server=Pid}}=State)->
	io:format("table server is down"),
	NewState=State#state{table=undefined},
	{reply,{text,err_json(?ENTER,<<"table_server_disconnected">>)},Req,NewState};
websocket_info(Info,Req,State)->
	io:format("info is ~p~n",[Info]),
	{ok,Req,State}.

websocket_terminate(Reason,_Req,#state{table=undefined})->
	io:format("terminated reason is ~p~n",[Reason]),
	ok;
websocket_terminate(Reason,_Req,#state{table=#table{server=Server}})->
	io:format("terminated reason is ~p~n",[Reason]),
	dealer_game_api:disconnect(Server),
	ok.