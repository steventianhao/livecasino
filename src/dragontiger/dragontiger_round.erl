-module(dragontiger_round).
-include("round.hrl").

-export([set_betting/1,set_done/2,new_shoe/1]).


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

set_betting(OldRound)->
	RoundIndex=OldRound#round.roundIndex+1,
	ShoeIndex =OldRound#round.shoeIndex,
	#round{createTime=casino_utils:now(),roundIndex=RoundIndex,shoeIndex=ShoeIndex}.
	
set_done(Round,Cards)->
	Round#round{cards=Cards}.