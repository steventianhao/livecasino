-module(player_game_api_tests).
-include_lib("eunit/include/eunit.hrl").

check_bets_test()->
	Cats=[1000,1001,1002,1003,1004,1005,1006,1007,1008,1009,1010],
	Amounts=[1,2,3,4,5,6,7,8,9,10,11],
	?assert(player_game_api:check_bets(Cats,Amounts,baccarat)).

check_bets2_test()->
	Cats=[2000,2001,2002,2003,2004,2005,2006],
	Amounts=[1,2,3,4,5,6,7],
	?assert(player_game_api:check_bets(Cats,Amounts,dragontiger)).
