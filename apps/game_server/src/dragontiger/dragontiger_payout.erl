-module(dragontiger_payout).
-export([ratio/1,reward/1,payout/1,result/1]).

-include("dragontiger.hrl").

value(#card{rank=?ACE})->1;
value(#card{rank=?TWO})->2;
value(#card{rank=?THREE})->3;
value(#card{rank=?FOUR})->4;
value(#card{rank=?FIVE})->5;
value(#card{rank=?SIX})->6;
value(#card{rank=?SEVEN})->7;
value(#card{rank=?EIGHT})->8;
value(#card{rank=?NINE})->9;
value(#card{rank=?TEN})->10;
value(#card{rank=?JACK})->11;
value(#card{rank=?QUEEN})->12;
value(#card{rank=?KING})->13.

ratio(dragon)-> 
	{?BET_DRAGON,2,?BET_DRAGON};
ratio(tiger)-> 
	{?BET_TIGER,2,?BET_TIGER};
ratio(tie)->
	{?BET_TIE,9,?BET_TIE};
ratio(dragon_odd)->
	{?BET_DRAGON_ODD,2,?BET_DRAGON_ODD};
ratio(tiger_odd)->
	{?BET_TIGER_ODD,2,?BET_TIGER_ODD};
ratio(dragon_even)->
	{?BET_DRAGON_EVEN,2,?BET_DRAGON_EVEN};
ratio(tiger_even)->
	{?BET_TIGER_EVEN,2,?BET_TIGER_EVEN};
ratio(tiger_tie)-> 
	{?BET_TIGER,0.5,?BET_TIE};
ratio(dragon_tie) ->
	{?BET_DRAGON,0.5,?BET_TIE}.

is_odd(7)->
	false;
is_odd(V)->
	V rem 2 ==1.
is_even(V)->
	V rem 2 ==0.

reward(#{?DRAGON_POS := D, ?TIGER_POS := T})->
	Dv=value(D),
	Tv=value(T),
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

result(Cards)->
	Fun=fun(E)-> {_,_,B}=ratio(E),B end,
	lists:map(Fun,reward(Cards)).
	
payout(Cards=#{})->
	Fun=fun(E)->{B,R,_}=ratio(E),{B,R} end,
	maps:from_list(lists:map(Fun,reward(Cards))).