-module(player_action_handler).
-export([handle_msg/2,auth_json/0]).
-include("handler_state.hrl").

-define(AUTH,<<"auth">>).
-define(TOKEN,<<"token">>).
-define(ENTER,<<"enter">>).
-define(TABLE,<<"table">>).
-define(BET,<<"bet">>).
-define(CATS,<<"cats">>).
-define(AMOUNTS,<<"amounts">>).
-define(CMD,<<"cmd">>).
-define(CODE,<<"code">>).
-define(ERROR,<<"error">>).
-define(INVALID,<<"invalid">>).

%fake token data for now
-define(TOKENS,#{<<"1">> =>{1,<<"simon">>}, <<"2">> => {2,<<"valor">>},<<"3">> => {3,<<"jacy">>}}).

%entry function, decode Msg and transform it to map
handle_msg(Msg,State)->
	case jsx:is_json(Msg) of
		true->
			Action=maps:from_list(jsx:decode(Msg)),
			handle_action(Action,State);
		false->
			{err_json(?INVALID),State}
	end.

% 1,authenticate token via internal authentication server
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

% 2,join the table, new one player process on  the game server or reuse the previous one
% get the current information about bets, cards, rounds, payouts if any
handle_action(#{?CMD := ?ENTER,?TABLE := Table},#state{player={Id,Name},tables=Tables}=State)->
	case maps:find(Table,Tables) of
		{ok,_}->
			{ok_json(?ENTER),State};
		error ->
			case player_game_api:find_server(Table) of
				undefined->
					{err_json(?ENTER,<<"game_server_not_found">>),State};
				GameServerPid->
					case player_game_api:join(GameServerPid,{user,Id,Name},Table) of
						{ok,{_PlayerPid,_GameName,_Payout}=Result}->
							{ok_json(?ENTER),State#state{tables=maps:put(Table,Result,Tables)}};
						{error,Error}->
							{err_json(?ENTER,Error),State}
					end
			end
	end;

% 3,quit the table
% just disconnect from the player process, but player process still there if any bets placed on this round or last round

% 4,user quit(quit all tables),same as above

% 5,bet on one table
% check the cats and amounts first, then send to player process.
handle_action(#{?CMD:=?BET,?TABLE:=Table,?CATS:=Cats,?AMOUNTS:=Amounts},#state{tables=Tables}=State)->
	case maps:find(Table,Tables) of
		{ok,{PlayerPid,GameName,_}}->
			case player_game_api:bet(PlayerPid,GameName,Cats,Amounts) of
				{ok,{BundleId,BalanceAfter}}->
					{ok_json(?BET,[{<<"bundle_id">>,BundleId},{<<"balance_after">>,BalanceAfter}]),State};
				{error,Error}->
					{err_json(?BET,Error),State};
				error ->
					{err_json(?BET),State}
			end;
		error ->
			{err_json(?BET,<<"enter_table_first">>),State}
	end;


% catch all, log the information when dev, when in production, close the connection.
handle_action(Req,State)->
	io:format("this is we got in handle_action(catch all) ~p~n",[Req]),
	{err_json(?INVALID),State}.


% helper functions.

handle_auth(Token)->
	case maps:find(Token,?TOKENS) of
		{ok,User}->
			{ok,User};
		_->
			error
	end.

%json functions
auth_json()->
	jsx:encode([{?CMD,?AUTH}]).

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