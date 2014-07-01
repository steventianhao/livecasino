-module(baccarat_player_mod).
-include("baccarat.hrl").
-export([payout/2,is_valid_bets/2]).

is_valid_bets(Cats,Amounts)->
	casino_bets:is_valid_bets(Cats,Amounts,?ALL_BET_CATS).

payout(Cards=#{},commission) when map_size(Cards) >= 4 ->
	baccarat_payout_commission:payout(Cards);
payout(Cards=#{},nocommission) when map_size(Cards) >=4 ->
	baccarat_payout_nocommission:payout(Cards).