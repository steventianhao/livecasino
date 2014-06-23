-module(baccarat_payout_commission).
-export([ratio/1,reward/1,payout/1]).
-include("baccarat.hrl").

ratio(banker)->
	{?BET_BANKER,1.95};
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
	{?BET_PLAYER,1}.

is_pair(#card{rank=R1},#card{rank=R2})->
	R1==R2.

reward_morethan4(Bs=[B1,B2|_],Ps=[P1,P2|_])->
	Pt=casino_bets:baccarat_total(Ps),
	Bt=casino_bets:baccarat_total(Bs),
	R1=if
		Pt==Bt ->
			[tie,banker_tie,player_tie];
		Pt > Bt ->
			[player];
		Pt < Bt ->
			[banker]
	end,
	R2=casino_bets:add_reward(R1,is_pair(B1,B2),banker_pair),
	R3=casino_bets:add_reward(R2,is_pair(P1,P2),player_pair),
	[big|R3].

reward(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,
		 ?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2}=Cards) when map_size(Cards)==4 ->
	Pt=casino_bets:baccarat_total([P1,P2]),
	Bt=casino_bets:baccarat_total([B1,B2]),
	R1=if
		Pt == Bt ->
			[tie,banker_tie,player_tie];
		Pt > Bt ->
			if 
				Pt ==8 ->
					[player,player_n8];
				Pt ==9 ->
					[player,player_n9];
				true ->
					[player]
			end;
		Pt < Bt ->
			if
				Bt==8 ->
					[banker,banker_n8];
				Bt==9 ->
					[banker,banker_n9];
				true ->
					[banker]
			end
	end,
	R2=casino_bets:add_reward(R1,is_pair(B1,B2),banker_pair),
	R3=casino_bets:add_reward(R2,is_pair(P1,P2),player_pair),
	[small|R3];

reward(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,?BANKER_POS_3 := B3,
		 ?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2}=Cards) when map_size(Cards)==5 ->
	reward_morethan4([B1,B2,B3],[P1,P2]);

reward(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,?BANKER_POS_3 := B3,
		 ?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2,?PLAYER_POS_3 := P3}=Cards) when map_size(Cards)==6->
	reward_morethan4([B1,B2,B3],[P1,P2,P3]);

reward(#{?BANKER_POS_1 := B1,?BANKER_POS_2 := B2,
		 ?PLAYER_POS_1 := P1,?PLAYER_POS_2 := P2,?PLAYER_POS_3 := P3}=Cards) when map_size(Cards)==5->
	reward_morethan4([B1,B2],[P1,P2,P3]).


payout(Cards=#{})->
	casino_bets:payout(Cards,fun ratio/1,fun reward/1).