-module(player_handler).
-behavior(gen_event).

-compile([{parse_transform, lager_transform}]).
-record(state,{pid}).
-include("dealer.hrl").

-export([init/1,handle_event/2,handle_call/2,code_change/3,terminate/2,handle_info/2]).

dealer_to_json(Dealer) when is_list(Dealer)->
	#{dealer=>list_to_binary(Dealer)};
dealer_to_json(Dealer) when is_record(Dealer,dealer)->
	#{dealer=>list_to_binary(Dealer#dealer.name)}.

init(Pid)->
	{ok,#state{pid=Pid}}.

dealer_event(Kind,Table,Dealer,Pid)->
	M=maps:merge(#{kind=>Kind,table=>Table},dealer_to_json(Dealer)),
	Pid ! {json,jsx:encode(M)}.
	
handle_event({dealer_connect,{Table,Dealer}},State=#state{pid=Pid})->
	dealer_event(dealer_connect,Table,Dealer,Pid),
	{ok,State};

handle_event({dealer_disconnect,{Table,Dealer}},State=#state{pid=Pid})->
	dealer_event(dealer_disconnect,Table,Dealer,Pid),
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