-module(dealer_ws_handler).
-behaviour(cowboy_websocket_handler).

-define(KIND,<<"kind">>).
-define(DEALERS,#{<<"simon">>=><<"111111">>,<<"valor">>=><<"222222">>}).

-record(state,{auth=false,table=undefined}).

-export([init/3,websocket_init/3,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

init({tcp,http},_Req,_Otps)->
	{upgrade,protocol,cowboy_websocket}.

websocket_init(_TransportName,Req,_Otps)->
	self()! auth,
	{ok,Req,#state{}}.

websocket_handle({text,Msg},Req,State)->
	{_,Json,NewState}=handle_msg(Msg,State),
	{reply,{text,Json},Req,NewState};
websocket_handle(_Data,Req,State)->
	{ok,Req,State}.

websocket_info(auth,Req,State)->
	Json=jsx:encode([{?KIND,<<"auth">>}]),
	{reply,{text,Json},Req,State};
websocket_info(Info,Req,State)->
	io:format("info is ~p~n",[Info]),
	{ok,Req,State}.

websocket_terminate(Reason,_Req,_State)->
	io:format("terminated reason is ~p~n",[Reason]),
	ok.

invalid_response(State)->
	Json=jsx:encode([{?KIND,<<"invalid">>}]),
	{error,Json,State}.

handle_msg(Msg,State)->
	io:format("this is msg we got ~p~n",[Msg]),
	case jsx:is_json(Msg) of
		true-> 
			handle_json(jsx:decode(Msg),State);
		false->
			invalid_response(State)
	end.

handle_json(Tuples,State)->
	Map=maps:from_list(Tuples),
	io:format("this is map we get from json ~p~n",[Map]),
	handle_action(Map,State).
	
handle_action(#{?KIND := <<"auth">>, <<"username">> := Username, <<"password">> := Password}=Req,#state{auth=false}=State)->
	io:format("this is we got in handle_action(auth) ~p~n",[Req]),
	case handle_auth(Username,Password) of
		true->
			Json=jsx:encode([{<<"kind">>,<<"auth_ok">>}]),
			{ok,Json,State#state{auth=true}};
		false->
			Json=jsx:encode([{<<"kind">>,<<"auth_error">>}]),
			{ok,Json,State}
	end;
	
handle_action(#{?KIND := <<"enter">>, <<"table">> := _Table}=Req,#state{auth=true}=State)->
	Json=jsx:encode(Req),
	{ok,Json,State};

handle_action(Req,State)->
	io:format("this is we got in handle_action(catch all) ~p~n",[Req]),
	invalid_response(State).


handle_auth(Username,Password)->
	case maps:find(Username,?DEALERS) of
		{ok,P} when P==Password ->
			true;
		_->
			false
	end.