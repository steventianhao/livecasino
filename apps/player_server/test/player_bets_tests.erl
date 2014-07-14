-module(player_bets_tests).
-include_lib("eunit/include/eunit.hrl").

is_valid_bets_test()->
	?assertNot(player_bets:is_valid_bets([],[1],[2])),
	?assertNot(player_bets:is_valid_bets([1],[],[2])),
	?assertNot(player_bets:is_valid_bets([1],[2],[])),
	?assertNot(player_bets:is_valid_bets([1],[3,4],[2])).
	
is_valid_bets2_test()->
	AllBetCats=[1,2,3],
	?assertNot(player_bets:is_valid_bets([1,2,4],[3.0,4.0,5.0],AllBetCats)),
	?assertNot(player_bets:is_valid_bets([1,2,2],[3.0,4.0,5.0],AllBetCats)),
	?assert(player_bets:is_valid_bets([1,2],[3.0,4.0],AllBetCats)).