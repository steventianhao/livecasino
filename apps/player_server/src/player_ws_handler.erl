-module(player_ws_handler).
-behaviour(cowboy_websocket_handler).
-record(state,{player=undefined,tables=#{}}).

-export([init/3,websocket_init/3,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

-define(AUTH,<<"auth">>).
-define(TOKEN,<<"token">>).
-define(ENTER,<<"enter">>).
-define(TABLE,<<"table">>).

-define(CMD,<<"cmd">>).
-define(CODE,<<"code">>).
-define(ERROR,<<"error">>).
-define(INVALID,<<"invalid">>).

init({tcp,http},_Req,_Otps)->
	{upgrade,protocol,cowboy_websocket}.


websocket_init(_TransportName,Req,_Otps)->
	self() ! auth,
	{ok,Req,#state{}}.

websocket_handle({text,Msg},Req,State)->
	{Json,NewState}=case jsx:is_json(Msg) of
		true->
			Action=maps:from_list(jsx:decode(Msg)),
			handle_action(Action,State);
		false->
			{err_json(?INVALID),State}
	end,
	{reply,{text,Json},Req,NewState};
	
websocket_handle(_Data,Req,State)->
	{ok,Req,State}.

websocket_info(auth,Req,State)->
	{reply,{text,json(?AUTH)},Req,State};

websocket_info({json,Json},Req,State)->
	{reply,{text,Json},Req,State};

websocket_info(Info,Req,State)->
	io:format("info is ~p~n",[Info]),
	{ok,Req,State}.

websocket_terminate(Reason,_Req,_State)->
	io:format("terminated reason is ~p~n",[Reason]),
	ok.

json(Cmd)->
	jsx:encode([{?CMD,Cmd}]).

ok_json(Cmd)->
	jsx:encode([{?CMD,Cmd},{?CODE,1}]).
ok_json(Cmd,Others)->
	jsx:encode([{?CMD,Cmd},{?CODE,1}|Others]).

error_json(Cmd,Code)->
	jsx:encode([{?CMD,Cmd},{?CODE,Code}]).
error_json(Cmd,Code,Error)->
	jsx:encode([{?CMD,Cmd},{?CODE,Code},{?ERROR,Error}]).

err_json(Cmd)->
	error_json(Cmd,-1).
err_json(Cmd,Error)->
	error_json(Cmd,-1,Error).

-define(TOKENS,#{<<"1">> =>{1,<<"simon">>}, <<"2">> => {2,<<"valor">>},<<"3">> => {3,<<"jacy">>}}).
handle_auth(Token)->
	case maps:find(Token,?TOKENS) of
		{ok,User}->
			{ok,User};
		_->
			error
	end.


handle_action(#{?CMD := ?AUTH, ?TOKEN := Token},#state{player=undefined}=State)->
	case handle_auth(Token) of
		{ok,{Id,Username}} ->
			case global:register_name({user,Id},self()) of
				yes->
					{ok_json(?AUTH),State#state{player={Id,Username}}};
				no ->
					{err_json(?AUTH,<<"muliple_login">>),State}
			end;
		error->
			{err_json(?AUTH),State}
	end;

handle_action(_Req,#state{player=undefined}=State)->
	{err_json(?AUTH),State};

% handle_action(#{?CMD := ?ENTER,?TABLE := Table},#state{player={Id,Name},tables=#{}})->
% 	case maps:find(Table,tables) of
% 		{ok,_}->
% 			{ok_json(?ENTER),State};
% 		error ->
% 			case player_game_api:find_server(Table) of
% 				undefined->
% 					{err_json(?ENTER),State}
% 				Pid->
% 					player_game_api:join(Pid,{user,Id,Name},Table)
		



handle_action(Req,State)->
	io:format("this is we got in handle_action(catch all) ~p~n",[Req]),
	{err_json(?INVALID),State}.