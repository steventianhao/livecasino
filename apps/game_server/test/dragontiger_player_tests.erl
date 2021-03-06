-module(dragontiger_player_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/dragontiger/dragontiger.hrl").
-include("card.hrl").

payout_test()->
	Cards=#{?DRAGON_POS=>#card{rank=?ACE},?TIGER_POS=>#card{rank=?ACE}},
	Payout=dragontiger_player_mod:payout(Cards,dragontiger),
	?assert(map_size(Payout)==5),
	?assertMatch(#{?BET_DRAGON := 0.5,?BET_TIGER :=0.5,?BET_TIE := 9,?BET_DRAGON_ODD:=2,?BET_TIGER_ODD:=2},Payout).

payout2_test()->
	Cards=#{?DRAGON_POS=>#card{rank=?TWO},?TIGER_POS=>#card{rank=?ACE}},
	Payout=dragontiger_player_mod:payout(Cards,dragontiger),
	?assert(map_size(Payout)==3),
	?assertMatch(#{?BET_DRAGON := 2,?BET_DRAGON_EVEN:=2,?BET_TIGER_ODD:=2},Payout).

payout3_test()->
	Cards=#{?DRAGON_POS=>#card{rank=?SEVEN},?TIGER_POS=>#card{rank=?ACE}},
	Payout=dragontiger_player_mod:payout(Cards,dragontiger),
	?assert(map_size(Payout)==2),
	?assertMatch(#{?BET_DRAGON := 2,?BET_TIGER_ODD:=2},Payout).

payout4_test()->
	Cards=#{?DRAGON_POS=>#card{rank=?SEVEN},?TIGER_POS=>#card{rank=?TEN}},
	Payout=dragontiger_player_mod:payout(Cards,dragontiger),
	?assert(map_size(Payout)==2),
	?assertMatch(#{?BET_TIGER := 2,?BET_TIGER_EVEN:=2},Payout).
