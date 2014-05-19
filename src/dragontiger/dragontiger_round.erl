-module(dragontiger_round).
-include("round.hrl").

-export([restore_round/6,set_dealing/1,set_betting/2,set_done/2,new_shoe/1]).

restore_round(ShoeIndex,RoundIndex,Status,Cards,CreateTime,FinishTime)->
	if
		Status /= ?DONE orelse Status /=?DEALING orelse Status /=?BETTING ->
			{ok,#round{shoeIndex=ShoeIndex,roundIndex=RoundIndex,createTime=CreateTime,finishTime=FinishTime,status=Status,cards=Cards}};
		true->
			error
	end.

increase(ShoeIndex,_CreateTime)->
	ShoeIndex+1.

new_shoe(undefined)->
	#round{shoeIndex=1,roundIndex=0};
new_shoe(#round{shoeIndex=undefined})->
	#round{shoeIndex=1,roundIndex=0};
new_shoe(Round=#round{roundIndex=0})->
	Round;
new_shoe(#round{shoeIndex=ShoeIndex,createTime=CreateTime})->
	NewShoeIndex=increase(ShoeIndex,CreateTime),
	#round{shoeIndex=NewShoeIndex,roundIndex=0}.

set_betting(OldRound,Dealer)->
	RoundIndex=OldRound#round.roundIndex+1,
	ShoeIndex =OldRound#round.shoeIndex,
	#round{status=?BETTING,createTime=erlang:now(),dealer=Dealer,roundIndex=RoundIndex,shoeIndex=ShoeIndex}.
	
set_dealing(Round)->
	Round#round{status=?DEALING}.

set_done(Round,Cards)->
	Round#round{status=?DONE,finishTime=erlang:now(),cards=Cards}.