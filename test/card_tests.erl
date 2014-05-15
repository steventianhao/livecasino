-module(card_tests).
-include_lib("eunit/include/eunit.hrl").
-define(DEBUG,true).

results()->
	["HQD7#HAH5C5","S7D7#H9HK","CTD7#HJH8","C4D4D7#H6ST","D9D7#CTHKD2","CTD7#HKC4DJ","DAD7#H4S3","DJD7#DKS9",
	"HKD6D7#S3H3SK","DAS4D7#C2H2","SAD7#S3C5","D4S7D7#C9SKHA","D2C8D7#H8C2DQ","HAD7#C2H9","HAS7D7#H6DAH2",
	"DKD7#H8D3SJ","S9D8D7#H4H2C9","S9D7#DQSQDT","H2DTD2#DKSQCJ","D4C3D8#DTC2H8","S2C3C9#S9DAD9","DQDKSQ#D5D2DT","D5HQHT#H9C6D4",
	"C9C2CT#H9C8C2","S6SQST#H6STC6","H7CQCT#STSQHJ","C3HQS5#D2SQSJ","S5HQSJ#C4SAHK","DJH3SJ#D3DADJ",
	"H6DKDA#CKSQCT","D4C4H7#DQDKDQ","S2CJS5#DTHTDJ","C2HJCJ#CQHKD2","C4D7C5#D6DASA","HQHAHQ#S3S3SJ",
	"SQC8H3#D7D3C9","CKDKC4#CKHAST","S3DACT#SKS2D8","S5DTCQ#S4H5D7","S3DACT#SKS2D8"].

check_cards(Cards)->
	[Pcs,Bcs]=string:tokens(Cards,"#"),
	card:check_cards(Pcs) andalso card:check_cards(Bcs).

check_cards_test()->
	Result=lists:all(fun(L)-> check_cards(L) end,results()),
	?assert(Result).

check_cards2_test()->
	?assert(card:check_cards([])).

check_cards3_test()->
	?assertNot(card:check_cards(["DKD"])).

check_cards4_test()->
	?assertNot(card:check_cards(abc)).

check_cards5_test()->
	?assertNot(card:check_cards([a,b])).
