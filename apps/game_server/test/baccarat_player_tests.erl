-module(baccarat_player_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/baccarat/baccarat.hrl").

payout_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?ACE},?BANKER_POS_2=>#card{rank=?ACE},
			?PLAYER_POS_1=>#card{rank=?ACE},?PLAYER_POS_2=>#card{rank=?SEVEN}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),4),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_N8 :=9,?BET_SMALL := 2.45,?BET_BANKER_PAIR := 12},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),4),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_N8 :=9,?BET_SMALL := 2.45,?BET_BANKER_PAIR :=12},Payout2).

payout2_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?ACE},?BANKER_POS_2=>#card{rank=?ACE},
			?PLAYER_POS_1=>#card{rank=?ACE},?PLAYER_POS_2=>#card{rank=?EIGHT}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),4),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_N9 :=9,?BET_SMALL := 2.45,?BET_BANKER_PAIR := 12},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),4),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_N9 :=9,?BET_SMALL := 2.45,?BET_BANKER_PAIR :=12},Payout2).

payout3_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?EIGHT},?BANKER_POS_2=>#card{rank=?ACE},
			?PLAYER_POS_1=>#card{rank=?ACE},?PLAYER_POS_2=>#card{rank=?SEVEN}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),3),
	?assertMatch(#{?BET_BANKER := 1.95,?BET_BANKER_N9 :=9,?BET_SMALL := 2.45},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),3),
	?assertMatch(#{?BET_BANKER := 2,?BET_BANKER_N9 :=9,?BET_SMALL := 2.45},Payout2).

payout4_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?SEVEN},?BANKER_POS_2=>#card{rank=?ACE},
			?PLAYER_POS_1=>#card{rank=?ACE},?PLAYER_POS_2=>#card{rank=?TWO}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),3),
	?assertMatch(#{?BET_BANKER := 1.95,?BET_BANKER_N8 :=9,?BET_SMALL := 2.45},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),3),
	?assertMatch(#{?BET_BANKER := 2,?BET_BANKER_N8 :=9,?BET_SMALL := 2.45},Payout2).

payout5_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?SEVEN},?BANKER_POS_2=>#card{rank=?FIVE},?BANKER_POS_3=>#card{rank=?FOUR},
			?PLAYER_POS_1=>#card{rank=?QUEEN},?PLAYER_POS_2=>#card{rank=?TEN},?PLAYER_POS_3=>#card{rank=?FIVE}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),2),
	?assertMatch(#{?BET_BANKER := 1.95,?BET_BIG := 1.53},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),2),
	?assertMatch(#{?BET_BANKER := 1.5,?BET_BIG := 1.53},Payout2).

payout6_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?TWO},?BANKER_POS_2=>#card{rank=?TWO},
			?PLAYER_POS_1=>#card{rank=?SEVEN},?PLAYER_POS_2=>#card{rank=?FOUR},?PLAYER_POS_3=>#card{rank=?ACE}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),3),
	?assertMatch(#{?BET_BANKER := 1.95,?BET_BIG := 1.53,?BET_BANKER_PAIR:=12},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),3),
	?assertMatch(#{?BET_BANKER := 2,?BET_BIG := 1.53,?BET_BANKER_PAIR:=12},Payout2).

payout7_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?THREE},?BANKER_POS_2=>#card{rank=?FOUR},
			?PLAYER_POS_1=>#card{rank=?SEVEN},?PLAYER_POS_2=>#card{rank=?TEN}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),4),
	?assertMatch(#{?BET_TIE :=9,?BET_SMALL := 2.45,?BET_BANKER:=1,?BET_PLAYER:=1},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),4),
	?assertMatch(#{?BET_BANKER := 1,?BET_PLAYER:=1,?BET_SMALL := 2.45,?BET_TIE:=9},Payout2).

payout8_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?FIVE},?BANKER_POS_2=>#card{rank=?FIVE},?BANKER_POS_3=>#card{rank=?ACE},
			?PLAYER_POS_1=>#card{rank=?SEVEN},?PLAYER_POS_2=>#card{rank=?QUEEN}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(3,map_size(Payout)),
	?assertMatch(#{?BET_BIG := 1.53,?BET_PLAYER:=2,?BET_BANKER_PAIR:=12},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(3,map_size(Payout2)),
	?assertMatch(#{?BET_BIG:=1.53,?BET_PLAYER:=2,?BET_BANKER_PAIR:=12},Payout2).

payout9_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?KING},?BANKER_POS_2=>#card{rank=?ACE},?BANKER_POS_3=>#card{rank=?FOUR},
			?PLAYER_POS_1=>#card{rank=?JACK},?PLAYER_POS_2=>#card{rank=?QUEEN},?PLAYER_POS_3=>#card{rank=?FIVE}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),4),
	?assertMatch(#{?BET_BANKER := 1,?BET_PLAYER:=1,?BET_BIG := 1.53,?BET_TIE:=9},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),4),
	?assertMatch(#{?BET_BANKER := 1,?BET_PLAYER:=1,?BET_BIG := 1.53,?BET_TIE:=9},Payout2).

payout10_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?ACE},?BANKER_POS_2=>#card{rank=?KING},?BANKER_POS_3=>#card{rank=?NINE},
			?PLAYER_POS_1=>#card{rank=?SEVEN},?PLAYER_POS_2=>#card{rank=?SEVEN},?PLAYER_POS_3=>#card{rank=?FOUR}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),3),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_PAIR:=12,?BET_BIG := 1.53},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),3),
	?assertMatch(#{?BET_PLAYER := 2,?BET_PLAYER_PAIR:=12,?BET_BIG := 1.53},Payout2).

payout11_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?SEVEN},?BANKER_POS_2=>#card{rank=?NINE},
			?PLAYER_POS_1=>#card{rank=?SEVEN},?PLAYER_POS_2=>#card{rank=?KING}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),2),
	?assertMatch(#{?BET_PLAYER := 2,?BET_SMALL := 2.45},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),2),
	?assertMatch(#{?BET_PLAYER := 2,?BET_SMALL := 2.45},Payout2).

payout12_test()->
	Cards=#{?BANKER_POS_1=>#card{rank=?SEVEN},?BANKER_POS_2=>#card{rank=?KING},
			?PLAYER_POS_1=>#card{rank=?SEVEN},?PLAYER_POS_2=>#card{rank=?NINE}},
	Payout=baccarat_player_mod:payout(Cards,commission),
	?assertEqual(map_size(Payout),2),
	?assertMatch(#{?BET_BANKER := 1.95,?BET_SMALL := 2.45},Payout),

	Payout2=baccarat_player_mod:payout(Cards,nocommission),
	?assertEqual(map_size(Payout2),2),
	?assertMatch(#{?BET_BANKER := 2,?BET_SMALL := 2.45},Payout2).
