-module(casino_card_tests).
-include_lib("eunit/include/eunit.hrl").
-include("card.hrl").

cards_to_string_test()->
	Cards=[#card{suit=?HEART,rank=?ACE},#card{suit=?CLUB,rank=?QUEEN}],
	?assertEqual("HACQ",casino_card:cards_to_string(Cards)).


binary_to_card_test()->
	S = <<"D6">>,
	?assertEqual(#card{suit=?DIAMOND,rank=?SIX},casino_card:binary_to_card(S)).

card_to_binary_test()->
	S = <<"D6">>,
	?assertEqual(S,casino_card:card_to_binary(#card{suit=?DIAMOND,rank=?SIX})).