-module(casino_events).
-export([subscribe/1,publish/2]).

-define(PROPERTY(Id),{p,l,{dealer_table,Id}}).

subscribe(DealerTableId)->
	gproc:reg(?PROPERTY(DealerTableId)).

publish(DealerTableId,Msg)->
	gproc:send(?PROPERTY(DealerTableId),Msg).