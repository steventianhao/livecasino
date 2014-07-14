-module(casino_card_tests).
-include_lib("eunit/include/eunit.hrl").
-include("card.hrl").

cards_to_string_test()->
	Cards=[#card{suit=?HEART,rank=?ACE},#card{suit=?CLUB,rank=?QUEEN}],
	?assertEqual("HACQ",casino_card:cards_to_string(Cards)).


one_card_test()->
	S = <<"D6">>,
	?assertEqual(#card{suit=?DIAMOND,rank=?SIX},casino_card:one_card(S)).