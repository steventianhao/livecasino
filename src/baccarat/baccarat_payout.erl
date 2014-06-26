-module(baccarat_payout).
-include("baccarat.hrl").
-export([reward4/1,reward4plus/2]).

result4(Pt,Bt) when Pt==Bt ->
	[tie,banker_tie,player_tie];
result4(Pt=8,Bt) when Pt>Bt ->
	[player,player_n8];
result4(Pt=9,Bt) when Pt>Bt ->
	[player,player_n9];
result4(Pt,Bt) when Pt>Bt ->
	[player];
result4(Pt,Bt=8) when Pt<Bt ->
	[banker,banker_n8];
result4(Pt,Bt=9) when Pt<Bt ->
	[banker,banker_n9];
result4(Pt,Bt) when Pt<Bt ->
	[banker].

reward4(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,
		?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2}=Cards) when map_size(Cards)==4 ->
	Pt=casino_card:total([P1,P2]),
	Bt=casino_card:total([B1,B2]),
	R1=result4(Pt,Bt),
	R2=casino_bets:add_reward(R1,casino_card:is_pair(B1,B2),banker_pair),
	R3=casino_bets:add_reward(R2,casino_card:is_pair(P1,P2),player_pair),
	[small|R3].

reward_morethan4(Bs=[B1,B2|_],Ps=[P1,P2|_],Func)->
	Pt=casino_card:total(Ps),
	Bt=casino_card:total(Bs),
	R1=Func(Pt,Bt),
	R2=casino_bets:add_reward(R1,casino_card:is_pair(B1,B2),banker_pair),
	R3=casino_bets:add_reward(R2,casino_card:is_pair(P1,P2),player_pair),
	[big|R3].

reward4plus(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,?BANKER_POS_3 := B3,
		?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2}=Cards,Func) when map_size(Cards)==5 ->
	reward_morethan4([B1,B2,B3],[P1,P2],Func);

reward4plus(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,?BANKER_POS_3 := B3,
		?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2,?PLAYER_POS_3 := P3}=Cards,Func) when map_size(Cards)==6->
	reward_morethan4([B1,B2,B3],[P1,P2,P3],Func);

reward4plus(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,
		?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2,?PLAYER_POS_3 := P3}=Cards,Func) when map_size(Cards)==5->
	reward_morethan4([B1,B2],[P1,P2,P3],Func).

