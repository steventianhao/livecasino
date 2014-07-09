-module(dealer_ws_handler).
-behaviour(cowboy_websocket_handler).

-define(KIND,<<"kind">>).
-define(AUTH,<<"auth">>).
-define(ENTER,<<"enter">>).
-define(DEAL,<<"deal">>).
-define(CLEAR,<<"clear">>).
-define(POS,<<"pos">>).
-define(CARD,<<"card">>).
-define(SCAN,<<"scan">>).
-define(TABLE,<<"table">>).
-define(USERNAME,<<"username">>).
-define(PASSWORD,<<"password">>).


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
	{reply,{text,auth_json()},Req,State};
websocket_info(Info,Req,State)->
	io:format("info is ~p~n",[Info]),
	{ok,Req,State}.

websocket_terminate(Reason,_Req,_State)->
	io:format("terminated reason is ~p~n",[Reason]),
	ok.

invalid_json()->
	jsx:encode([{?KIND,<<"invalid">>}]).
	
auth_json()->
	jsx:encode([{?KIND,<<"auth_needed">>}]).

handle_msg(Msg,State)->
	io:format("this is msg we got ~p~n",[Msg]),
	case jsx:is_json(Msg) of
		true-> 
			handle_json(jsx:decode(Msg),State);
		false->
			{error,invalid_json(),State}
	end.

handle_json(Tuples,State)->
	Map=maps:from_list(Tuples),
	io:format("this is map we get from json ~p~n",[Map]),
	handle_action(Map,State).
	
handle_action(#{?KIND := ?AUTH,  ?USERNAME:= Username,  ?PASSWORD:= Password}=Req,#state{auth=false}=State)->
	io:format("this is we got in handle_action(auth) ~p~n",[Req]),
	case handle_auth(Username,Password) of
		true->
			Json=jsx:encode([{?KIND,<<"auth_ok">>}]),
			{ok,Json,State#state{auth=true}};
		false->
			Json=jsx:encode([{?KIND,<<"auth_error">>}]),
			{ok,Json,State}
	end;

handle_action(_Req,#state{auth=false}=State)->	
	{ok,auth_json(),State};

handle_action(#{?KIND := ?ENTER, ?TABLE := Table},State)->
	case global:whereis_name({game_server,Table}) of
		undefined-> 
			Json=jsx:encode([{?KIND,<<"enter_error">>}]),
			{ok,Json,State};
		Pid->
			Json=jsx:encode([{?KIND,<<"enter_ok">>}]),
			{ok,Json,State#state{table=Pid}}
	end;

handle_action(_Req,#state{table=undefined}=State)->
	{ok,jsx:encode([{?KIND,<<"enter_needed">>}]),State};

handle_action(#{?KIND := ?DEAL, ?CARD := _Card,?POS := _Pos}=Req,#state{table=_Table}=State)->
	{ok,jsx:encode(Req),State};

handle_action(#{?KIND := ?CLEAR,?POS:= _Pos}=Req,#state{table=_Table}=State)->
	{ok,jsx:encode(Req),State};

handle_action(#{?KIND := ?SCAN,?CARD:= _Card}=Req,#state{table=_Table}=State)->
	{ok,jsx:encode(Req),State};

handle_action(Req,State)->
	io:format("this is we got in handle_action(catch all) ~p~n",[Req]),
	{error,invalid_json(),State}.

handle_auth(Username,Password)->
	case maps:find(Username,?DEALERS) of
		{ok,P} when P==Password ->
			true;
		_->
			false
	end.