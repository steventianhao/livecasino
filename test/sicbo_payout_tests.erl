-module(sicbo_payout_tests).
-include_lib("eunit/include/eunit.hrl").

payout_test()->
	R=sicbo_payout:payout([1,2,2]),
	E=#{3002=>2,3001=>2,3005=>19,3022=>3,3021=>2,3042=>9,3051=>6},
	?assertEqual(E,R).

payout2_test()->
	R=sicbo_payout:payout([6,6,6]),
	E=#{3003=>2,3026=>4,3037=>25,3036=>151,3046=>9},
	?assertEqual(E,R).


payout3_test()->
	R=sicbo_payout:payout([4,5,6]),
	E=#{3000=>2,3002=>2,3015=>15,3024=>2,3025=>2,3026=>2,3063=>6,3064=>6,3065=>6},
	?assertEqual(E,R).
