-module(baccarat_player).
-behavior(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([bet/3]).

bet(Pid,Cats,Amounts)->
	gen_server:handle_call(Pid,{bet,Cats,Amounts}).

-record(state,{game,table,user,payout}).
-define(GAME_SERVER_EVENT_BUS,baccarat_game_eventbus).

init({Table,User,Payout})->
	%% add the listener handler
	gen_event:add_handler(?GAME_SERVER_EVENT_BUS,baccarat_player_handler,self()),
	{ok,#state{table=Table,user=User,payout=Payout}}.


handle_call(_Event={bet,Cats,Amounts},_From,State)->
	Result=baccarat_game_api:bet(Cats,Amounts),
	{reply,Result,State}.

handle_cast(Request,State)->
	lager:error("unexpected Request ~p, State ~p",[Request,State]),
	{noreply,State}.

handle_info({json,Json},State)->
	lager:info("json ~p, state ~p",[Json,State]),
	{noreply,State};
handle_info(Info,State)->
	lager:error("unexpected Info ~p, State ~p",[Info,State]),
	{noreply,State}.

terminate(Reason,State)->
	lager:info("terminate, Reason ~p, State ~p",[Reason,State]),
	gen_event:delete_handler(?GAME_SERVER_EVENT_BUS,baccarat_player_handler,self()),
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.
