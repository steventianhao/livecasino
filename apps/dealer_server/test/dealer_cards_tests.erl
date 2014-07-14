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

results()->
	["HQD7#HAH5C5","S7D7#H9HK","CTD7#HJH8","C4D4D7#H6ST","D9D7#CTHKD2","CTD7#HKC4DJ","DAD7#H4S3","DJD7#DKS9",
	"HKD6D7#S3H3SK","DAS4D7#C2H2","SAD7#S3C5","D4S7D7#C9SKHA","D2C8D7#H8C2DQ","HAD7#C2H9","HAS7D7#H6DAH2",
	"DKD7#H8D3SJ","S9D8D7#H4H2C9","S9D7#DQSQDT","H2DTD2#DKSQCJ","D4C3D8#DTC2H8","S2C3C9#S9DAD9","DQDKSQ#D5D2DT","D5HQHT#H9C6D4",
	"C9C2CT#H9C8C2","S6SQST#H6STC6","H7CQCT#STSQHJ","C3HQS5#D2SQSJ","S5HQSJ#C4SAHK","DJH3SJ#D3DADJ",
	"H6DKDA#CKSQCT","D4C4H7#DQDKDQ","S2CJS5#DTHTDJ","C2HJCJ#CQHKD2","C4D7C5#D6DASA","HQHAHQ#S3S3SJ",
	"SQC8H3#D7D3C9","CKDKC4#CKHAST","S3DACT#SKS2D8","S5DTCQ#S4H5D7","S3DACT#SKS2D8"].

check_cards(Cards)->
	[Pcs,Bcs]=string:tokens(Cards,"#"),
	dealer_cards:check_cards(Pcs) andalso dealer_cards:check_cards(Bcs).

check_cards_test()->
	Result=lists:all(fun(L)-> check_cards(L) end,results()),
	?assert(Result).

check_cards2_test()->
	?assert(not dealer_cards:check_cards([])).

check_cards3_test()->
	?assertNot(dealer_cards:check_cards(["DKD"])).

check_cards4_test()->
	?assertNot(dealer_cards:check_cards(abc)).

check_cards5_test()->
	?assertNot(dealer_cards:check_cards([a,b])).