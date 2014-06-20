-module(casino_shoe_round).
-include("round.hrl").
-include("db.hrl").

-export([new_round/1,new_shoe/1,persist_round/3]).
-define(CASINO_DB,mysql_casino_master).

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

new_round(OldRound)->
	RoundIndex=OldRound#round.roundIndex+1,
	ShoeIndex =OldRound#round.shoeIndex,
	#round{createTime=casino_utils:now(),roundIndex=RoundIndex,shoeIndex=ShoeIndex}.

persist_round(NewRound,DealerId,Table)->
	#round{createTime={Mills,_},roundIndex=RoundIndex,shoeIndex=ShoeIndex}=NewRound,
	DbNewRound=#db_new_round_req{shoe_index=ShoeIndex,round_index=RoundIndex,dealer_id=DealerId,dealer_table_id=Table,create_time=Mills},
    mysql_db:insert_round(?CASINO_DB,DbNewRound).