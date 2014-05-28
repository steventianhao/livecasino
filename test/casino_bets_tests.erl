-module(casino_bets_tests).
-include_lib("eunit/include/eunit.hrl").

create_bet_req_test()->
	R=casino_bets:create_bet_req(123,45,3,[1,2],[56.02,78.01]),
	?debugFmt("result of create_bet_req ~p",[R]),
	?assert({db_bet_req,123,45,3,"1,2","56.02,78.01",134.03}=:=R).
