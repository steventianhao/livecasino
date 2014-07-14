-module(dealer_cards_tests).
-include_lib("eunit/include/eunit.hrl").

check_card_test()->
	?assertEqual(false,dealer_cards:check_one_card(abc)),
	?assertEqual(false,dealer_cards:check_one_card("H")),
	?assertEqual(false,dealer_cards:check_one_card("HTC")),
	?assertEqual(false,dealer_cards:check_one_card("XK")),
	?assertEqual(false,dealer_cards:check_one_card("DH")),
	?assertEqual(false,dealer_cards:check_one_card("55")),
	?assert(dealer_cards:check_one_card("HT")),
	?assert(dealer_cards:check_one_card("S5")).

check_card2_test_()->
	Suits="DCHS",
	Ranks="23456789TJQKA",
	[?_assert(dealer_cards:check_one_card([S,R]))||S<-Suits,R<-Ranks].

check_pos1_test()->
	?assert(dealer_cards:check_pos(1,dragontiger)),
	?assert(dealer_cards:check_pos(2,dragontiger)),
	?assert(not dealer_cards:check_pos(3,dragontiger)),
	?assert(not dealer_cards:check_pos(abc,dragontiger)).

check_pos2_test()->
	?assert(dealer_cards:check_pos(1,baccarat)),
	?assert(dealer_cards:check_pos(2,baccarat)),
	?assert(dealer_cards:check_pos(3,baccarat)),
	?assert(dealer_cards:check_pos(4,baccarat)),
	?assert(dealer_cards:check_pos(5,baccarat)),
	?assert(dealer_cards:check_pos(6,baccarat)),
	?assert(not dealer_cards:check_pos(7,baccarat)),
	?assert(not dealer_cards:check_pos(abc,baccarat)).