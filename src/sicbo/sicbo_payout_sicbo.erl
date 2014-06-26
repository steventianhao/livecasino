-module(sicbo_payout_sicbo).

-export([payout/1]).

-record(range_bet,{id,ratio,min,max}).
-record(bet,{id,ratio}).
-record(total_bet,{id,ratio,total}).
-record(number_bet,{id,ratio,num}).
-record(pair_bet,{id,ratio,nums}).

-define(BIG,#range_bet{id=3000,ratio=1,min=11,max=17}).
-define(SMALL,#range_bet{id=3001,ratio=1,min=4,max=10}).

-define(ODD,#bet{id=3002,ratio=1}).
-define(EVEN,#bet{id=3003,ratio=1}).

-define(TOTAL4,#total_bet{id=3004,ratio=50,total=4}).
-define(TOTAL5,#total_bet{id=3005,ratio=18,total=5}).
-define(TOTAL6,#total_bet{id=3006,ratio=14,total=6}).
-define(TOTAL7,#total_bet{id=3007,ratio=12,total=7}).
-define(TOTAL8,#total_bet{id=3008,ratio=8,total=8}).
-define(TOTAL9,#total_bet{id=3009,ratio=6,total=9}).
-define(TOTAL10,#total_bet{id=3010,ratio=6,total=10}).
-define(TOTAL11,#total_bet{id=3011,ratio=6,total=11}).
-define(TOTAL12,#total_bet{id=3012,ratio=6,total=12}).
-define(TOTAL13,#total_bet{id=3013,ratio=8,total=13}).
-define(TOTAL14,#total_bet{id=3014,ratio=12,total=14}).
-define(TOTAL15,#total_bet{id=3015,ratio=14,total=15}).
-define(TOTAL16,#total_bet{id=3016,ratio=18,total=16}).
-define(TOTAL17,#total_bet{id=3017,ratio=50,total=17}).

payout_total(Total)->
	[].

-define(NUMBER1,#number_bet{id=3021,ratio=1,num=1}).
-define(NUMBER2,#number_bet{id=3022,ratio=1,num=2}).
-define(NUMBER3,#number_bet{id=3023,ratio=1,num=3}).
-define(NUMBER4,#number_bet{id=3024,ratio=1,num=4}).
-define(NUMBER5,#number_bet{id=3025,ratio=1,num=5}).
-define(NUMBER6,#number_bet{id=3026,ratio=1,num=6}).
-define(NUMBERS,[?NUMBER1,?NUMBER2,?NUMBER3,?NUMBER4,?NUMBER5,?NUMBER6]).

payout_number([D1,D1,D1])->
	[{Id,Ratio*3} ||#number_bet{id=Id,ratio=Ratio,num=Num}<-?NUMBERS,Num==D1];
payout_number([D1,D1,D2])->
	[].

-define(TRIPLE1,#number_bet{id=3031,ratio=150,num=1}).
-define(TRIPLE2,#number_bet{id=3032,ratio=150,num=2}).
-define(TRIPLE3,#number_bet{id=3033,ratio=150,num=3}).
-define(TRIPLE4,#number_bet{id=3034,ratio=150,num=4}).
-define(TRIPLE5,#number_bet{id=3035,ratio=150,num=5}).
-define(TRIPLE6,#number_bet{id=3036,ratio=150,num=6}).

-define(TRIPLES,[?TRIPLE1,?TRIPLE2,?TRIPLE3,?TRIPLE4,?TRIPLE5,?TRIPLE6]).
-define(ANY_TRIPLE,#bet{id=3037,ratio=24}).

payout_triple(D1,D1,D1)->
	Ts=[{Id,Ratio} || #number_bet{id=Id,ratio=Ratio,num=Num}<-?TRIPLES,Num==D1],
	[{?ANY_TRIPLE#number_bet.id,?ANY_TRIPLE#number_bet.ratio}|Ts];
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
	[{Id,Ratio}||#pair_bet{id=Id,ratio=Ratio,nums=Nums}<-?PAIRS,sets:from_list(Nums)==Dss].

payout([D1,D2,D3]=Ds)->
	Pair=payout_pair(D1,D2)++payout_pair(D2,D3)++payout_pair(D1,D3),
	Triple=payout_triple(D1,D2,D3),
	Number=payout_number(lists:sort([D1,D2,D3])),
	Total=payout_total(lists:sum(Ds)),
	Pair++Triple++Number+Total.
