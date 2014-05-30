-module(dragontiger_player_mod).
-export([is_valid_bets/2,payout/2]).

-include("dragontiger.hrl").

is_valid_bets(Cats,Amounts)->
	casino_bets:is_valid_bets(Cats,Amounts,?ALL_BET_CATS).

is_odd(7)->
	false;
is_odd(V)->
	V rem 2 ==1.
is_even(V)->
	V rem 2 ==0.

add_reward(Rewards,Pass,Result) when Pass==true ->
	[Result | Rewards];
add_reward(Rewards,_,_)->
	Rewards.


payout(#{?DRAGON_POS := #card{value=Dv}, ?TIGER_POS := #card{value=Tv}},dragontiger)->
	R1=if
		Dv == Tv -> 
			[tie,tiger_tie,dragon_tie];
		Dv > Tv ->
			[dragon];
		Dv < Tv ->
			[tiger]
	end,
	R2=add_reward(R1,is_odd(Dv),dragon_odd),
	R3=add_reward(R2,is_even(Dv),dragon_even),
	R4=add_reward(R3,is_odd(Tv),tiger_odd),
	add_reward(R4,is_even(Tv),tiger_even).