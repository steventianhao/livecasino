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
	Cards=#{?BANKER_POS_1=>?SEVEN,?BANKER_POS_2=>?FIVE,?BANKER_POS_3=>?FOUR,
			?PLAYER_POS_1=>?QUEEN,?PLAYER_POS_2=>?TEN,?PLAYER_POS_3=>?FIVE},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),2),
	?assertMatch(#{?BET_BANKER := 1.95,?BET_BIG := 1.53},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),2),
	?assertMatch(#{?BET_BANKER := 1.5,?BET_BIG := 1.53},Payout2).

payout6_test()->
	Cards=#{?BANKER_POS_1=>?TWO,?BANKER_POS_2=>?TWO,
			?PLAYER_POS_1=>?SEVEN,?PLAYER_POS_2=>?FOUR,?PLAYER_POS_3=>?ACE},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),3),
	?assertMatch(#{?BET_BANKER := 1.95,?BET_BIG := 1.53,?BET_BANKER_PAIR:=12},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),3),
	?assertMatch(#{?BET_BANKER := 2,?BET_BIG := 1.53,?BET_BANKER_PAIR:=12},Payout2).

payout7_test()->
	Cards=#{?BANKER_POS_1=>?THREE,?BANKER_POS_2=>?FOUR,
			?PLAYER_POS_1=>?SEVEN,?PLAYER_POS_2=>?TEN},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),4),
	?assertMatch(#{?BET_TIE :=9,?BET_SMALL := 2.45,?BET_BANKER:=1,?BET_PLAYER:=1},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),4),
	?assertMatch(#{?BET_BANKER := 1,?BET_PLAYER:=1,?BET_SMALL := 2.45,?BET_TIE:=9},Payout2).

payout8_test()->
	Cards=#{?BANKER_POS_1=>?FIVE,?BANKER_POS_2=>?FIVE,?BANKER_POS_3=>?ACE,
			?PLAYER_POS_1=>?SEVEN,?PLAYER_POS_2=>?QUEEN},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(3,map_size(Payout)),
	?assertMatch(#{?BET_BIG := 1.53,?BET_PLAYER:=2,?BET_BANKER_PAIR:=12},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(3,map_size(Payout2)),
	?assertMatch(#{?BET_BIG:=1.53,?BET_PLAYER:=2,?BET_BANKER_PAIR:=12},Payout2).

payout9_test()->
	Cards=#{?BANKER_POS_1=>?KING,?BANKER_POS_2=>?ACE,?BANKER_POS_3=>?FOUR,
			?PLAYER_POS_1=>?JACK,?PLAYER_POS_2=>?QUEEN,?PLAYER_POS_3=>?FIVE},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),4),
	?assertMatch(#{?BET_BANKER := 1,?BET_PLAYER:=1,?BET_BIG := 1.53,?BET_TIE:=9},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),4),
	?assertMatch(#{?BET_BANKER := 1,?BET_PLAYER:=1,?BET_BIG := 1.53,?BET_TIE:=9},Payout2).

payout10_test()->
	Cards=#{?BANKER_POS_1=>?ACE,?BANKER_POS_2=>?KING,?BANKER_POS_3=>?NINE,
			?PLAYER_POS_1=>?SEVEN,?PLAYER_POS_2=>?SEVEN,?PLAYER_POS_3=>?FOUR},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),3),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_PAIR:=12,?BET_BIG := 1.53},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),3),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_PAIR:=12,?BET_BIG := 1.53},Payout2).

payout11_test()->
	Cards=#{?BANKER_POS_1=>?SEVEN,?BANKER_POS_2=>?NINE,
			?PLAYER_POS_1=>?SEVEN,?PLAYER_POS_2=>?KING},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),2),
	?assertMatch(#{?BET_PLAYER := 2,?BET_SMALL := 2.45},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),2),
	?assertMatch(#{?BET_PLAYER := 2,?BET_SMALL := 2.45},Payout2).

payout12_test()->
	Cards=#{?BANKER_POS_1=>?SEVEN,?BANKER_POS_2=>?KING,
			?PLAYER_POS_1=>?SEVEN,?PLAYER_POS_2=>?NINE},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),2),
	?assertMatch(#{?BET_BANKER := 1.95,?BET_SMALL := 2.45},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),2),
	?assertMatch(#{?BET_BANKER := 2,?BET_SMALL := 2.45},Payout2).
