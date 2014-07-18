-module(dragontiger_player_mod).
-export([payout/2]).

payout(Cards,dragontiger)->
	dragontiger_payout:payout(Cards).
	