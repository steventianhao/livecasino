-module(dragontiger_player_handler).
-behavior(gen_event).

-record(state,{listener,game,table}).
-include("dealer.hrl").

-export([init/1,handle_event/2,handle_call/2,code_change/3,terminate/2,handle_info/2]).

dealer_to_json(Dealer) when is_list(Dealer)->
	#{dealer=>list_to_binary(Dealer)};
dealer_to_json(Dealer) when is_record(Dealer,dealer)->
	#{dealer=>list_to_binary(Dealer#dealer.name)}.

init(Pid)->
	{ok,#state{listener=Pid}}.

dealer_event(Kind,Dealer,Pid)->
	M=maps:merge(#{kind=>Kind},dealer_to_json(Dealer)),
	Pid ! {json,jsx:encode(M)}.
	
handle_event({dealer_connect,Dealer},State=#state{listener=Pid})->
	dealer_event(dealer_connect,Dealer,Pid),
	{ok,State};

handle_event({dealer_disconnect,Dealer},State=#state{listener=Pid})->
	dealer_event(dealer_disconnect,Dealer,Pid),
	{ok,State};

handle_event(Event,State=#state{listener=Pid}) ->
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