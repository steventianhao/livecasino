-module(casino_card_tests).
-include_lib("eunit/include/eunit.hrl").
-include("card.hrl").
-include("../src/baccarat/baccarat.hrl").

c(Rank)->
	#card{rank=Rank}.
c(Suit,Rank)->
	#card{suit=Suit,rank=Rank}.

cards_to_string_test()->
	Cards=[c(?HEART,?ACE),c(?CLUB,?QUEEN)],
	?assertEqual("HACQ",casino_card:cards_to_string(Cards)).

remove_test()->
	M=#{?PLAYER_POS_1=>c(?JACK),?PLAYER_POS_2=>c(?KING)},
	?assert({ok,#{?PLAYER_POS_2=>c(?KING)}}=:=casino_card:remove(?PLAYER_POS_1,M)),
	?assert(error=:=casino_card:remove(?BANKER_POS_2,M)).

binary_to_card_test()->
	S = <<"D6">>,
	?assertEqual(c(?DIAMOND,?SIX),casino_card:binary_to_card(S)).

card_to_binary_test()->
	S = <<"D6">>,
	?assertEqual(S,casino_card:card_to_binary(c(?DIAMOND,?SIX))).