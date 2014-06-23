-module(baccarat_player_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/baccarat/baccarat.hrl").

payout_test()->
	Cards=#{?BANKER_POS_1=>?ACE,?BANKER_POS_2=>?ACE,?PLAYER_POS_1=>?ACE,?PLAYER_POS_2=>?SEVEN},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),4),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_N8 :=9,?BET_SMALL := 2.45,?BET_BANKER_PAIR := 12},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),4),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_N8 :=9,?BET_SMALL := 2.45,?BET_BANKER_PAIR :=12},Payout2).

payout2_test()->
	Cards=#{?BANKER_POS_1=>?ACE,?BANKER_POS_2=>?ACE,?PLAYER_POS_1=>?ACE,?PLAYER_POS_2=>?EIGHT},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),4),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_N9 :=9,?BET_SMALL := 2.45,?BET_BANKER_PAIR := 12},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),4),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_N9 :=9,?BET_SMALL := 2.45,?BET_BANKER_PAIR :=12},Payout2).

payout3_test()->
	Cards=#{?BANKER_POS_1=>?EIGHT,?BANKER_POS_2=>?ACE,?PLAYER_POS_1=>?ACE,?PLAYER_POS_2=>?SEVEN},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),3),
	?assertMatch(#{?BET_BANKER := 1.95,?BET_BANKER_N9 :=9,?BET_SMALL := 2.45},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),3),
	?assertMatch(#{?BET_BANKER := 2,?BET_BANKER_N9 :=9,?BET_SMALL := 2.45},Payout2).

payout4_test()->
	Cards=#{?BANKER_POS_1=>?SEVEN,?BANKER_POS_2=>?ACE,?PLAYER_POS_1=>?ACE,?PLAYER_POS_2=>?TWO},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),3),
	?assertMatch(#{?BET_BANKER := 1.95,?BET_BANKER_N8 :=9,?BET_SMALL := 2.45},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),3),
	?assertMatch(#{?BET_BANKER := 2,?BET_BANKER_N8 :=9,?BET_SMALL := 2.45},Payout2).

payout5_test()->
	Cards=#{?BANKER_POS_1=>?SEVEN,?BANKER_POS_2=>?FIVE,?BANKER_POS_3=>?FOUR,?PLAYER_POS_1=>?QUEEN,?PLAYER_POS_2=>?TEN,?PLAYER_POS_3=>?FIVE},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),2),
	?assertMatch(#{?BET_BANKER := 1.95,?BET_BIG := 1.53},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),2),
	?assertMatch(#{?BET_BANKER := 1.5,?BET_BIG := 1.53},Payout2).