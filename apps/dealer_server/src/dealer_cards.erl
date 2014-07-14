-module(dealer_cards).
-export([check_one_card/1,check_pos/2]).
-export([check_deal/3,check_clear/2,check_scan/2]).

-define(ALL_SUIT,[$H,$S,$D,$C]).
-define(ALL_RANK,[$A,$2,$3,$4,$5,$6,$7,$8,$9,$T,$J,$Q,$K]).

check_one_card([S,R]) when is_integer(S) andalso is_integer(R)->
	lists:member(S,?ALL_SUIT) andalso lists:member(R,?ALL_RANK);
check_one_card(_)->
	false.

check_pos(Pos,dragontiger)->
	dealer_dragontiger:check_pos(Pos);
check_pos(Pos,baccarat)->
	dealer_baccarat:check_pos(Pos).

check_deal(Game,Pos,Card) when is_integer(Pos) andalso is_binary(Card)->
	check_pos(Pos,Game) andalso check_one_card(binary_to_list(Card));
check_deal(_Game,_Pos,_Card)->
	false.

check_clear(Game,Pos) when is_integer(Pos)->
	check_pos(Pos,Game);
check_clear(_Game,_Pos)->
	false.

check_scan(Card) when is_binary(Card)->
	check_one_card(binary_to_list(Card));
check_scan(_Card)->
	false.