-module(dragontiger_payout_dragontiger).
-export([ratio/1,reward/1,payout/1]).

-include("dragontiger.hrl").

ratio(dragon)-> 
	{?BET_DRAGON,2};
ratio(tiger)-> 
	{?BET_TIGER,2};
ratio(tie)->
	{?BET_TIE,9};
ratio(dragon_odd)->
	{?BET_DRAGON_ODD,2};
ratio(tiger_odd)->
	{?BET_TIGER_ODD,2};
ratio(dragon_even)->
	{?BET_DRAGON_EVEN,2};
ratio(tiger_even)->
	{?BET_TIGER_EVEN,2};
ratio(tiger_tie)-> 
	{?BET_TIGER,0.5};
ratio(dragon_tie) ->
	{?BET_DRAGON,0.5}.


is_odd(7)->
	false;
is_odd(V)->
	V rem 2 ==1.
is_even(V)->
	V rem 2 ==0.

reward(#{?DRAGON_POS := #card{value=Dv}, ?TIGER_POS := #card{value=Tv}})->
	R1=if
		Dv == Tv -> 
			[tie,tiger_tie,dragon_tie];
		Dv > Tv ->
			[dragon];
		Dv < Tv ->
			[tiger]
	end,
	R2=casino_bets:add_reward(R1,is_odd(Dv),dragon_odd),
	R3=casino_bets:add_reward(R2,is_even(Dv),dragon_even),
	R4=casino_bets:add_reward(R3,is_odd(Tv),tiger_odd),
	casino_bets:add_reward(R4,is_even(Tv),tiger_even).

payout(Cards=#{?DRAGON_POS := #card{}, ?TIGER_POS := #card{}})->
	casino_bets:payout(Cards,fun ratio/1,fun reward/1).