-module(roulette_payout_roulette_tests).
-include_lib("eunit/include/eunit.hrl").

payout_test()->
	P=roulette_payout_roulette:payout(1),
	?assertEqual(#{},P).