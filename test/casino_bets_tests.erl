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

insert_bets_test()->
	Table=ets:new(bets,[set]),
	casino_bets:insert_bets(Table,1234,[1,2,3,4],[1.1,2.2,3.3,4.4]),
	R=ets:tab2list(Table),
	?debugFmt("records in ets ~p",[R]),
	ets:delete(Table),
	?assert(true).

payout_bets_test()->
	RatioMap=#{2000 => 2,2004 => 2,2005 => 2},
	Table=ets:new(bets,[set]),
	casino_bets:insert_bets(Table,1234,[2000,2001,2002,2003],[1.1,2.2,3.3,4.4]),
	casino_bets:payout_bets(Table,RatioMap),
	R=ets:tab2list(Table),
	?debugFmt("records in ets ~p",[R]),
	ets:delete(Table),
	?assert(true).	