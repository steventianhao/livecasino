-module(dragontiger_player_handler).
-behavior(gen_event).

-record(state,{pid}).
-include("dealer.hrl").

%% gen_event callbacks
-export([init/1,handle_event/2,handle_call/2,code_change/3,terminate/2,handle_info/2]).

%%API
-export([add_handler/2,del_handler/3]).


%%APIs
add_handler(EventBus,UserId) when is_pid(EventBus)->
	gen_event:add_handler(EventBus,{?MODULE,UserId},self()).
del_handler(EventBus,UserId,Reason)->
	gen_event:delete_handler(EventBus,{?MODULE,UserId},Reason).

%%private functions
dealer_to_json(Dealer) when is_list(Dealer)->
	#{dealer=>list_to_binary(Dealer)};
dealer_to_json(Dealer) when is_record(Dealer,dealer)->
	#{dealer=>list_to_binary(Dealer#dealer.name)}.


%%gen_event callbacks
init(Pid)->
	{ok,#state{pid=Pid}}.

dealer_event(Kind,Dealer,Pid)->
	M=maps:merge(#{kind=>Kind},dealer_to_json(Dealer)),
	Pid ! {json,jsx:encode(M)}.
	
handle_event({dealer_connect,Dealer},State=#state{pid=Pid})->
	dealer_event(dealer_connect,Dealer,Pid),
	{ok,State};

handle_event({dealer_disconnect,Dealer},State=#state{pid=Pid})->
	dealer_event(dealer_disconnect,Dealer,Pid),
	{ok,State};

handle_event(Event,State=#state{pid=Pid}) ->
	lager:info("module ~p, Event ~p",[?MODULE,Event]),
	Pid ! Event,
	{ok,State}.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.

handle_call(_Request,State)->
	{ok,ok,State}.

handle_info(_Info,State)->
	{ok,State}.

terminate(_Arg,_State)->
	ok.