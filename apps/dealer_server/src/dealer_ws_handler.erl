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

-record(table,{id,server,game}).
-record(state,{dealer=undefined,table=undefined}).

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
			Map=maps:from_list(jsx:decode(Msg)),
			handle_action(Map,State);
		false->
			{err_json(?INVALID),State}
	end,
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

ok_json(Kind)->
	jsx:encode([{?KIND,Kind},{?CODE,1}]).
ok_json(Kind,Others)->
	jsx:encode([{?KIND,Kind},{?CODE,1}|Others]).
err_json(Kind)->
	jsx:encode([{?KIND,Kind},{?CODE,-1}]).
err_json(Kind,Error)->
	jsx:encode([{?KIND,Kind},{?CODE,-1},{?ERROR,Error}]).

handle_action(#{?KIND := ?QUIT},#state{table=undefined}=State)->
	{ok_json(?QUIT),State#state{dealer=undefined}};

handle_action(#{?KIND := ?QUIT},#state{table=#table{server=Server}}=State)->
	dealer_game_api:disconnect(Server),
	{ok_json(?QUIT),State#state{dealer=undefined,table=undefined}};

handle_action(#{?KIND := ?AUTH,  ?USERNAME:= Username,  ?PASSWORD:= Password}=Req,#state{dealer=undefined}=State)->
	io:format("this is we got in handle_action(auth) ~p~n",[Req]),
	case handle_auth(Username,Password) of
		{ok,{Id,Username}}->
			{ok_json(?AUTH),State#state{dealer={Id,Username}}};
		error->
			{err_json(?AUTH),State}
	end;

handle_action(_Req,#state{dealer=undefined}=State)->	
	{err_json(?AUTH),State};

handle_action(#{?KIND := ?ENTER, ?TABLE := Table},#state{dealer=Dealer,table=undefined}=State)->
	case game_api:find_server(Table) of
		undefined-> 
			{err_json(?ENTER,<<"server_not_found">>),State};
		Pid->
			{Id,Username}=Dealer,
			%% should consider the condition like pid is not alive this moment, 
			%% and timeout(when timeout, then maybe sever accept this connection, 
			%% so should allow the same dealer enter into the same room multiple times).
			case game_api:connect(Pid,Id,Username) of
				{ok,Game} ->
					erlang:monitor(process,Pid),
					{ok_json(?ENTER),State#state{table=#table{id=Table,server=Pid,game=Game}}};
				{error,dealer_existed} ->
					{err_json(?ENTER,<<"other_dealer_existed">>),State}
			end
	end;

handle_action(#{?KIND := ?ENTER, ?TABLE := Table},#state{table=#table{id=Table2}}=State)->
	case Table of 
		Table2 ->
			{ok_json(?ENTER),State};
		_->
			{err_json(?ENTER,<<"table_connected">>),State}
	end;

handle_action(_Req,#state{table=undefined}=State)->
	{err_json(?ENTER),State};

handle_action(#{?KIND := ?NEWSHOE},#state{table=#table{server=Server}}=State)->
	handle_simple_action(?NEWSHOE,dealer_game_api:new_shoe(Server),State);

handle_action(#{?KIND := ?STARTBET},#state{table=#table{server=Server}}=State)->
	handle_simple_action(?STARTBET,dealer_game_api:start_bet(Server),State);

handle_action(#{?KIND := ?STOPBET},#state{table=#table{server=Server}}=State)->
	handle_simple_action(?STOPBET,dealer_game_api:stop_bet(Server),State);

handle_action(#{?KIND := ?COMMIT},#state{table=#table{server=Server}}=State)->
	handle_simple_action(?COMMIT,dealer_game_api:commit(Server),State);
	
handle_action(#{?KIND := ?DEAL, ?CARD := Card,?POS := Pos},#state{table=#table{server=Server,game=Game}}=State)->
	handle_simple_action(?DEAL,dealer_game_api:deal(Server,Game,Card,Pos),State);
	
handle_action(#{?KIND := ?CLEAR,?POS:= Pos},#state{table=#table{server=Server,game=Game}}=State)->
	handle_simple_action(?CLEAR,dealer_game_api:clear(Server,Game,Pos),State);
	
handle_action(#{?KIND := ?SCAN,?CARD:= Card},#state{table=#table{server=Server}}=State)->
	Json=case dealer_game_api:scan(Server,Card) of
		error ->
			err_json(?SCAN);
		{error,Error}->
			err_json(?SCAN,Error);
		{ok,Status,Pos}->
			ok_json(?SCAN,[{?POS,Pos},{<<"status">>,Status}])
	end,
	{Json,State};

handle_action(Req,State)->
	io:format("this is we got in handle_action(catch all) ~p~n",[Req]),
	{err_json(?INVALID),State}.

handle_simple_action(Kind,Result,State)->
	case Result of
		ok->
			{ok_json(Kind),State};
		error->
			{err_json(Kind),State};
		{error,Error}->
			{err_json(Kind,Error),State}
	end.

handle_auth(Username,Password)->
	case maps:find(Username,?DEALERS) of
		{ok,{Id,P}} when P==Password ->
			{ok,{Id,Username}};
		_->
			error
	end.