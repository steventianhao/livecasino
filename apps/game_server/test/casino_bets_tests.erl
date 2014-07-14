-module(casino_bets_tests).
-include_lib("eunit/include/eunit.hrl").

create_bet_req_test()->
	R=casino_bets:create_bet_req(123,45,3,[1,2],[56.02,78.01]),
	?assertEqual({db_bet_req,123,45,3,"1,2","56.02,78.01",134.03},R).

create_payout_req_test()->
	R=casino_bets:create_payout_req(123,45,3,#{2000=>23.45,2001=>45.67},23.45+45.67),
	?assertEqual({db_payout_req,123,45,3,"2000,2001","23.45,45.67",69.12},R).

insert_bets_test()->
	Table=ets:new(bets,[set]),
	casino_bets:insert_bets(Table,1234,[1,2,3,4],[1.1,2.2,3.3,4.4]),
	Result=ets:tab2list(Table),
	ets:delete(Table),
	?assert(length(Result)==4),
	?assert(lists:all(fun({{_BundleId,_C},_A,R})->R==0 end,Result)).

payout_bets_test()->
	RatioMap=#{2000 => 2,2004 => 2,2005 => 2},
	Table=ets:new(bets,[set]),
	casino_bets:insert_bets(Table,1234,[2000,2001,2002,2003],[1.1,2.2,3.3,4.4]),
	casino_bets:player_payout(Table,RatioMap),
	Result=ets:tab2list(Table),
	M=maps:from_list([{C,R}||{{_BundleId,C},_A,R} <-Result]),
	ets:delete(Table),
	?assertMatch(#{2000:=2,2001:=0,2002:=0,2003:=0},M).

player_payout_test()->
	RatioMap=#{2000 => 2,2004 => 2,2005 => 2},
	Table=ets:new(bets,[set]),
	casino_bets:insert_bets(Table,1234,[2000,2001,2002,2003],[1.1,2.2,3.3,4.4]),
	casino_bets:insert_bets(Table,2234,[2000,2004,2005,2003],[1.1,2.2,3.3,4.4]),
	{Pb,Pt}=casino_bets:player_payout(Table,RatioMap),
	ets:delete(Table),
	?assertMatch(#{1234:=2.2,2234:=13.2},Pb),
	?assertEqual(float_to_list(Pt,[{decimals,2},compact]),"15.4").