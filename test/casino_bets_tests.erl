-module(casino_bets_tests).
-include_lib("eunit/include/eunit.hrl").

create_bet_req_test()->
	R=casino_bets:create_bet_req(123,45,3,[1,2],[56.02,78.01]),
	?debugFmt("result of create_bet_req ~p",[R]),
	?assert({db_bet_req,123,45,3,"1,2","56.02,78.01",134.03}=:=R).


is_valid_bets_test()->
	?assertNot(casino_bets:is_valid_bets([],[1],[2])),
	?assertNot(casino_bets:is_valid_bets([1],[],[2])),
	?assertNot(casino_bets:is_valid_bets([1],[2],[])),
	?assertNot(casino_bets:is_valid_bets([1],[3,4],[2])).
	
is_valid_bets2_test()->
	AllBetCats=[1,2,3],
	?assertNot(casino_bets:is_valid_bets([1,2,4],[3.0,4.0,5.0],AllBetCats)),
	?assertNot(casino_bets:is_valid_bets([1,2,2],[3.0,4.0,5.0],AllBetCats)),
	?assert(casino_bets:is_valid_bets([1,2],[3.0,4.0],AllBetCats)).
	