-module(dragontiger_player_mod).
-export([is_valid_bets/2,payout/2]).

-include("dragontiger.hrl").

is_valid_bets(Cats,Amounts)->
	casino_bets:is_valid_bets(Cats,Amounts,?ALL_BET_CATS).

payout(Cards,dragontiger)->
	dragontiger_payout:payout(Cards).
	