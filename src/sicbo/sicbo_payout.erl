-module(sicbo_payout).
-export([payout/1,all_bets/0,all_bet_cats/0]).

-record(range_bet,{id,ratio,min,max}).
-record(bet,{id,ratio}).
-record(total_bet,{id,ratio,total}).
-record(number_bet,{id,ratio,num}).
-record(pair_bet,{id,ratio,nums}).

ratio_pair(Tuple)->
	{element(2,Tuple),element(3,Tuple)}.

-define(BIG,#range_bet{id=3000,ratio=1,min=11,max=17}).
-define(SMALL,#range_bet{id=3001,ratio=1,min=4,max=10}).
-define(ODD,#bet{id=3002,ratio=1}).
-define(EVEN,#bet{id=3003,ratio=1}).

-define(TOTAL04,#total_bet{id=3004,ratio=50,total=4}).
-define(TOTAL05,#total_bet{id=3005,ratio=18,total=5}).
-define(TOTAL06,#total_bet{id=3006,ratio=14,total=6}).
-define(TOTAL07,#total_bet{id=3007,ratio=12,total=7}).
-define(TOTAL08,#total_bet{id=3008,ratio=8,total=8}).
-define(TOTAL09,#total_bet{id=3009,ratio=6,total=9}).
-define(TOTAL10,#total_bet{id=3010,ratio=6,total=10}).
-define(TOTAL11,#total_bet{id=3011,ratio=6,total=11}).
-define(TOTAL12,#total_bet{id=3012,ratio=6,total=12}).
-define(TOTAL13,#total_bet{id=3013,ratio=8,total=13}).
-define(TOTAL14,#total_bet{id=3014,ratio=12,total=14}).
-define(TOTAL15,#total_bet{id=3015,ratio=14,total=15}).
-define(TOTAL16,#total_bet{id=3016,ratio=18,total=16}).
-define(TOTAL17,#total_bet{id=3017,ratio=50,total=17}).
-define(TOTALS,[?TOTAL04,?TOTAL05,?TOTAL06,?TOTAL07,?TOTAL08,?TOTAL09,?TOTAL10,
	?TOTAL11,?TOTAL12,?TOTAL13,?TOTAL14,?TOTAL15,?TOTAL16,?TOTAL17]).

payout_total(Total)->
	P=case (Total rem 2) of 
		1 -> 
			[?ODD];
		0 ->
			[?EVEN]
	end,
	R=[B||B<-[?BIG,?SMALL],B#range_bet.min =< Total,B#range_bet.max >= Total],
	T=[B||B<-?TOTALS,B#total_bet.total==Total],
	[ratio_pair(B)||B<-T++R++P].

-define(NUMBER1,#number_bet{id=3021,ratio=1,num=1}).
-define(NUMBER2,#number_bet{id=3022,ratio=1,num=2}).
-define(NUMBER3,#number_bet{id=3023,ratio=1,num=3}).
-define(NUMBER4,#number_bet{id=3024,ratio=1,num=4}).
-define(NUMBER5,#number_bet{id=3025,ratio=1,num=5}).
-define(NUMBER6,#number_bet{id=3026,ratio=1,num=6}).
-define(NUMBERS,[?NUMBER1,?NUMBER2,?NUMBER3,?NUMBER4,?NUMBER5,?NUMBER6]).

payout_number21(#number_bet{id=Id,ratio=Ratio,num=Two},Two,_One)->
	{true,{Id,Ratio*2}};
payout_number21(#number_bet{id=Id,ratio=Ratio,num=One},_Two,One)->
	{true,{Id,Ratio}};
payout_number21(_E,_Two,_One)->
	false.
payout_number([X,X,X])->
	[{Id,Ratio*3} ||#number_bet{id=Id,ratio=Ratio,num=Num}<-?NUMBERS,Num==X];
payout_number([X,X,Y])->
	Fun=fun(E)->payout_number21(E,X,Y) end,
	lists:filtermap(Fun,?NUMBERS);
payout_number([Y,X,X])->
	Fun=fun(E)->payout_number21(E,X,Y) end,
	lists:filtermap(Fun,?NUMBERS);
payout_number([X,Y,X])->
	Fun=fun(E)->payout_number21(E,X,Y) end,
	lists:filtermap(Fun,?NUMBERS);
payout_number(Nums)->
	[ratio_pair(B)||B<-?NUMBERS,lists:member(B#number_bet.num,Nums)].

-define(TRIPLE1,#number_bet{id=3031,ratio=150,num=1}).
-define(TRIPLE2,#number_bet{id=3032,ratio=150,num=2}).
-define(TRIPLE3,#number_bet{id=3033,ratio=150,num=3}).
-define(TRIPLE4,#number_bet{id=3034,ratio=150,num=4}).
-define(TRIPLE5,#number_bet{id=3035,ratio=150,num=5}).
-define(TRIPLE6,#number_bet{id=3036,ratio=150,num=6}).
-define(TRIPLES,[?TRIPLE1,?TRIPLE2,?TRIPLE3,?TRIPLE4,?TRIPLE5,?TRIPLE6]).
-define(ANY_TRIPLE,#bet{id=3037,ratio=24}).

payout_triple(D1,D1,D1)->
	Ts=[ratio_pair(B) || B<-?TRIPLES,B#number_bet.num==D1],
	[ratio_pair(?ANY_TRIPLE)|Ts];
payout_triple(_,_,_)->
	[].


-define(DOUBLE1,#pair_bet{id=3041,ratio=8,nums=[1]}).
-define(DOUBLE2,#pair_bet{id=3042,ratio=8,nums=[2]}).
-define(DOUBLE3,#pair_bet{id=3043,ratio=8,nums=[3]}).
-define(DOUBLE4,#pair_bet{id=3044,ratio=8,nums=[4]}).
-define(DOUBLE5,#pair_bet{id=3045,ratio=8,nums=[5]}).
-define(DOUBLE6,#pair_bet{id=3046,ratio=8,nums=[6]}).

-define(PAIR01,#pair_bet{id=3051,ratio=5,nums=[1,2]}).
-define(PAIR02,#pair_bet{id=3052,ratio=5,nums=[1,3]}).
-define(PAIR03,#pair_bet{id=3053,ratio=5,nums=[1,4]}).
-define(PAIR04,#pair_bet{id=3054,ratio=5,nums=[1,5]}).
-define(PAIR05,#pair_bet{id=3055,ratio=5,nums=[1,6]}).
-define(PAIR06,#pair_bet{id=3056,ratio=5,nums=[2,3]}).
-define(PAIR07,#pair_bet{id=3057,ratio=5,nums=[2,4]}).
-define(PAIR08,#pair_bet{id=3058,ratio=5,nums=[2,5]}).
-define(PAIR09,#pair_bet{id=3059,ratio=5,nums=[2,6]}).
-define(PAIR10,#pair_bet{id=3060,ratio=5,nums=[3,4]}).
-define(PAIR11,#pair_bet{id=3061,ratio=5,nums=[3,5]}).
-define(PAIR12,#pair_bet{id=3062,ratio=5,nums=[3,6]}).
-define(PAIR13,#pair_bet{id=3063,ratio=5,nums=[4,5]}).
-define(PAIR14,#pair_bet{id=3064,ratio=5,nums=[4,6]}).
-define(PAIR15,#pair_bet{id=3065,ratio=5,nums=[5,6]}).
-define(PAIRS,[?DOUBLE1,?DOUBLE2,?DOUBLE3,?DOUBLE4,?DOUBLE5,?DOUBLE6,?PAIR01,?PAIR02,?PAIR03,?PAIR04,?PAIR05,?PAIR06,?PAIR07,?PAIR08,?PAIR09,?PAIR10,?PAIR11,?PAIR12,?PAIR13,?PAIR14,?PAIR15]).

payout_pair(D1,D2)->
	Dss=sets:from_list([D1,D2]),
	[ratio_pair(B)||B<-?PAIRS,sets:from_list(B#pair_bet.nums)==Dss].
payout_pair(D1,D2,D3)->
	DL=payout_pair(D1,D2)++payout_pair(D2,D3)++payout_pair(D1,D3),
	sets:to_list(sets:from_list(DL)).

-define(ALL_BETS,?PAIRS++?TRIPLES++?NUMBERS++?TOTALS++[?ANY_TRIPLE,?ODD,?EVEN,?BIG,?SMALL]).
-define(ALL_BET_CATS,[element(2,B)||B<-?ALL_BETS]).

all_bets()->?ALL_BETS.
all_bet_cats()-> ?ALL_BET_CATS.

payout([D1,D2,D3]=Ds)->
	Pair=payout_pair(D1,D2,D3),
	Triple=payout_triple(D1,D2,D3),
	Number=payout_number(Ds),
	Total=payout_total(lists:sum(Ds)),
	Result=[{Id,Ratio+1}||{Id,Ratio}<-Pair++Triple++Number++Total],
	maps:from_list(Result).
