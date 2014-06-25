-module(baccarat_payout_nocommission).
-include("baccarat.hrl").
-export([ratio/1,reward/1,payout/1]).

ratio(banker)->
	{?BET_BANKER,2};
ratio(player)-> 
	{?BET_PLAYER,2};
ratio(tie) -> 
	{?BET_TIE,9};
ratio(banker_pair) -> 
	{?BET_BANKER_PAIR,12};
ratio(player_pair) ->
	{?BET_PLAYER_PAIR,12};
ratio(banker_n8)->
	{?BET_BANKER_N8,9};
ratio(banker_n9)->
	{?BET_BANKER_N9,9};
ratio(player_n8)->
	{?BET_PLAYER_N8,9};
ratio(player_n9)->
	{?BET_PLAYER_N9,9};
ratio(big)->
	{?BET_BIG,1.53};
ratio(small)->
	{?BET_SMALL,2.45};

ratio(banker_tie)->
	{?BET_BANKER,1};
ratio(player_tie) ->
	{?BET_PLAYER,1};

ratio(banker6)->
	{?BET_BANKER,1.5}.


is_pair(#card{rank=R1},#card{rank=R2})->
	R1==R2.

result4plus(Pt,Bt) when Pt==Bt ->
	[tie,banker_tie,player_tie];
result4plus(Pt,Bt) when Pt>Bt ->
	[player];
result4plus(Pt,Bt=6) when Pt<Bt ->
	[banker6];
result4plus(Pt,Bt) when Pt<Bt ->
	[banker].

reward_morethan4(Bs=[B1,B2|_],Ps=[P1,P2|_])->
	Pt=casino_card:total(Ps),
	Bt=casino_card:total(Bs),
	R1=result4plus(Pt,Bt),
	R2=casino_bets:add_reward(R1,is_pair(B1,B2),banker_pair),
	R3=casino_bets:add_reward(R2,is_pair(P1,P2),player_pair),
	[big|R3].

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


reward(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,
		?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2}=Cards) when map_size(Cards)==4 ->
	Pt=casino_card:total([P1,P2]),
	Bt=casino_card:total([B1,B2]),
	R1=result4(Pt,Bt),
	R2=casino_bets:add_reward(R1,is_pair(B1,B2),banker_pair),
	R3=casino_bets:add_reward(R2,is_pair(P1,P2),player_pair),
	[small|R3];

reward(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,?BANKER_POS_3 := B3,
		?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2}=Cards) when map_size(Cards)==5->
	reward_morethan4([B1,B2,B3],[P1,P2]);

reward(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,?BANKER_POS_3 := B3,
		?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2,?PLAYER_POS_3 := P3}=Cards) when map_size(Cards)==6->
	reward_morethan4([B1,B2,B3],[P1,P2,P3]);

reward(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,
		?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2,?PLAYER_POS_3 := P3}=Cards) when map_size(Cards)==5->
	reward_morethan4([B1,B2],[P1,P2,P3]).

payout(Cards=#{})->
	casino_bets:payout(Cards,fun ratio/1,fun reward/1).