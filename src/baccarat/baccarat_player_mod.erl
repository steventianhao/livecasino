-module(baccarat_player_mod).
-include("baccarat.hrl").
-export([reward/1,payout/2]).

add_reward(Rewards,Pass,Result) when Pass==true ->
	[Result | Rewards];
add_reward(Rewards,_,_)->
	Rewards.

total(Values) ->
	lists:sum(Values) rem 10.

reward_morethan4(Bt,Pt,B1,B2,P1,P2)->
	R1=if
		Pt==Bt ->
			[tie,banker_tie,player_tie];
		Pt > Bt ->
			[player];
		Pt < Bt ->
			if 
				Bt ==6 ->
					[banker6];
				true ->
					[banker]
			end
	end,
	R2=add_reward(R1,B1==B2,banker_pair),
	R3=add_reward(R2,P1==P2,player_pair),
	[big|R3].

reward(#{?BANKER_POS_1 := #card{value=B1},?BANKER_POS_2 := #card{value=B2},
		 ?PLAYER_POS_1 := #card{value=P1},?PLAYER_POS_2 := #card{value=P2}})->
	Pt=total([P1,P2]),
	Bt=total([B1,B2]),
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
				Bt==6 ->
					[banker6];
				Bt==8 ->
					[banker,banker_n8];
				Bt==9 ->
					[banker,banker_n9];
				true ->
					[banker]
			end
	end,
	R2=add_reward(R1,B1==B2,banker_pair),
	R3=add_reward(R2,P1==P2,player_pair),
	[small|R3];

reward(#{?BANKER_POS_1 := #card{value=B1},?BANKER_POS_2 := #card{value=B2},?BANKER_POS_3 := #card{value=B3},
		 ?PLAYER_POS_1 := #card{value=P1},?PLAYER_POS_2 := #card{value=P2}})->
	reward_morethan4(total([B1,B2,B3]),total([P1,P2]),B1,B2,P1,P2);

reward(#{?BANKER_POS_1 := #card{value=B1},?BANKER_POS_2 := #card{value=B2},?BANKER_POS_3 := #card{value=B3},
		 ?PLAYER_POS_1 := #card{value=P1},?PLAYER_POS_2 := #card{value=P2},?PLAYER_POS_3 := #card{value=P3}})->
	reward_morethan4(total([B1,B2,B3]),total([P1,P2,P3]),B1,B2,P1,P2);

reward(#{?BANKER_POS_1 := #card{value=B1},?BANKER_POS_2 := #card{value=B2},
		 ?PLAYER_POS_1 := #card{value=P1},?PLAYER_POS_2 := #card{value=P2},?PLAYER_POS_3 := #card{value=P3}})->
	reward_morethan4(total([B1,B2]),total([P1,P2,P3]),B1,B2,P1,P2).


payout(Cards=#{},commission)->
	Fun=fun baccarat_payout_commission:ratio/1,
	maps:from_list(lists:map(Fun,reward(Cards)));
payout(Cards=#{},nocommission)->
	Fun=fun baccarat_payout_nocommission:ratio/1,
	maps:from_list(lists:map(Fun,reward(Cards))).