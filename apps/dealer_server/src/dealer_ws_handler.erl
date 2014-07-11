-module(dealer_ws_handler).
-behaviour(cowboy_websocket_handler).

-define(KIND,<<"kind">>).
-define(CODE,<<"code">>).
-define(AUTH,<<"auth">>).
-define(ENTER,<<"enter">>).
-define(QUIT,<<"quit">>).
-define(DEAL,<<"deal">>).
-define(CLEAR,<<"clear">>).
-define(ERROR,<<"error">>).
-define(POS,<<"pos">>).
-define(CARD,<<"card">>).
-define(SCAN,<<"scan">>).
-define(TABLE,<<"table">>).
-define(NEWSHOE,<<"newshoe">>).
-define(STARTBET,<<"startbet">>).
-define(STOPBET,<<"stopbet">>).
-define(COMMIT,<<"commit">>).
-define(USERNAME,<<"username">>).
-define(PASSWORD,<<"password">>).
-define(INVALID,<<"invalid">>).

-define(DEALERS,#{<<"simon">>=>{1,<<"111111">>},<<"valor">>=>{2,<<"222222">>}}).

-record(state,{table_server=undefined,dealer=undefined,table_id=undefined}).
-export([init/3,websocket_init/3,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

init({tcp,http},_Req,_Otps)->
	{upgrade,protocol,cowboy_websocket}.

websocket_init(_TransportName,Req,_Otps)->
	self()! auth,
	{ok,Req,#state{}}.

websocket_handle({text,Msg},Req,State)->
	io:format("this is msg we got ~p~n",[Msg]),
	{Json,NewState}=case jsx:is_json(Msg) of
		true-> 
			handle_json(jsx:decode(Msg),State);
		false->
			{err_json(?INVALID),State}
	end,
	{reply,{text,Json},Req,NewState};
websocket_handle(_Data,Req,State)->
	{ok,Req,State}.

websocket_info(auth,Req,State)->
	{reply,{text,err_json(?AUTH)},Req,State};
websocket_info({'DOWN',_Ref,process,Pid,_},Req,#state{table_server=Pid}=State)->
	io:format("table server is down"),
	NewState=State#state{table_id=undefined,table_server=undefined},
	{reply,{text,err_json(?ENTER,<<"table_server_disconnected">>)},Req,NewState};
websocket_info(Info,Req,State)->
	io:format("info is ~p~n",[Info]),
	{ok,Req,State}.

websocket_terminate(Reason,_Req,#state{table_server=undefined})->
	io:format("terminated reason is ~p~n",[Reason]),
	ok;
websocket_terminate(Reason,_Req,#state{table_server=Table})->
	io:format("terminated reason is ~p~n",[Reason]),
	game_api:disconnect(Table),
	ok.

ok_json(Kind)->
	jsx:encode([{?KIND,Kind},{?CODE,1}]).
ok_json(Kind,Others)->
	jsx:encode([{?KIND,Kind},{?CODE,1}|Others]).
err_json(Kind)->
	jsx:encode([{?KIND,Kind},{?CODE,-1}]).
err_json(Kind,Error)->
	jsx:encode([{?KIND,Kind},{?CODE,-1},{?ERROR,Error}]).

handle_json(Tuples,State)->
	Map=maps:from_list(Tuples),
	io:format("this is map we get from json ~p~n",[Map]),
	handle_action(Map,State).

handle_action(#{?KIND := ?QUIT},#state{table_server=undefined}=State)->
	{ok_json(?QUIT),State#state{dealer=undefined}};

handle_action(#{?KIND := ?QUIT},#state{table_server=Table}=State)->
	game_api:disconnect(Table),
	{ok_json(?QUIT),State#state{dealer=undefined,table_id=undefined,table_server=undefined}};

handle_action(#{?KIND := ?AUTH,  ?USERNAME:= Username,  ?PASSWORD:= Password}=Req,#state{dealer=undefined}=State)->
	io:format("this is we got in handle_action(auth) ~p~n",[Req]),
	case handle_auth(Username,Password) of
		{ok,{Id,Username}}->
			{ok_json(?AUTH),State#state{dealer={Id,Username}}};
		error->
			{err_json(?AUTH),State}
	end;

handle_action(#{?KIND := ?AUTH,  ?USERNAME:= Username}=Req,#state{dealer={_Id,Username2}}=State)->
	io:format("this is we got in handle_action(auth) ~p~n",[Req]),
	if 
		Username =:= Username2 ->
			{ok_json(?AUTH),State};
		true ->
			{err_json(?AUTH,<<"other_dealer_authed">>),State}
	end;

handle_action(_Req,#state{dealer=undefined}=State)->	
	{err_json(?AUTH),State};

handle_action(#{?KIND := ?ENTER, ?TABLE := Table},#state{dealer=Dealer,table_id=undefined}=State)->
	case game_api:find_server(Table) of
		undefined-> 
			{err_json(?ENTER,<<"server_not_found">>),State};
		Pid->
			{Id,Username}=Dealer,
			%% should consider the condition like pid is not alive this moment, 
			%% and timeout(when timeout, then maybe sever accept this connection, 
			%% so should allow the same dealer enter into the same room multiple times).
			case game_api:connect(Pid,Id,Username) of
				ok ->
					erlang:monitor(process,Pid),
					{ok_json(?ENTER),State#state{table_server=Pid,table_id=Table}};
				{error,dealer_existed} ->
					{err_json(?ENTER,<<"other_dealer_existed">>),State}
			end
	end;

handle_action(#{?KIND := ?ENTER, ?TABLE := Table},#state{table_id=Table2}=State)->
	if
		Table=:=Table2 ->
			{ok_json(?ENTER),State};
		true->
			{err_json(?ENTER,<<"table_connected">>),State}
	end;

handle_action(_Req,#state{table_id=undefined}=State)->
	{err_json(?ENTER),State};

handle_action(#{?KIND := ?NEWSHOE},#state{table_server=Table}=State)->
	Json=case game_api:new_shoe(Table) of
		ok->
			ok_json(?NEWSHOE);
		Error ->
			err_json(?NEWSHOE,Error)
	end,
	{Json,State};

handle_action(#{?KIND := ?STARTBET},#state{table_server=Table}=State)->
	Json=case game_api:start_bet(Table) of
		ok ->
			ok_json(?STARTBET);
		Error->
			err_json(?STARTBET,Error)
	end,
	{Json,State};

handle_action(#{?KIND := ?STOPBET},#state{table_server=Table}=State)->
	Json=case game_api:stop_bet(Table) of
		ok ->
			ok_json(?STOPBET);
		Error ->
			err_json(?STOPBET,Error)
	end,
	{Json,State};

handle_action(#{?KIND := ?COMMIT},#state{table_server=Table}=State)->
	Json=case game_api:commit(Table) of
		ok ->
			ok_json(?COMMIT);
		Error->
			err_json(?COMMIT,Error)
	end,
	{Json,State};


handle_action(#{?KIND := ?DEAL, ?CARD := Card,?POS := Pos},#state{table_server=Table}=State)->
	Json=case game_api:deal(Table,Card,Pos) of
		ok ->
			ok_json(?DEAL);
		error ->
			err_json(?DEAL)
	end,
	{Json,State};

handle_action(#{?KIND := ?CLEAR,?POS:= Pos},#state{table_server=Table}=State)->
	Json=case game_api:clear(Table,Pos) of
		ok ->
			ok_json(?CLEAR);
		error ->
			err_json(?CLEAR)
	end,
	{Json,State};

handle_action(#{?KIND := ?SCAN,?CARD:= Card},#state{table_server=Table}=State)->
	Json=case game_api:scan(Table,Card) of
		error ->
			err_json(?SCAN);
		{Status,Pos}->
			ok_json(?SCAN,[{?POS,Pos},{<<"status">>,Status}])
	end,
	{Json,State};
	

handle_action(Req,State)->
	io:format("this is we got in handle_action(catch all) ~p~n",[Req]),
	{err_json(?INVALID),State}.

handle_auth(Username,Password)->
	case maps:find(Username,?DEALERS) of
		{ok,{Id,P}} when P==Password ->
			{ok,{Id,Username}};
		_->
			error
	end.