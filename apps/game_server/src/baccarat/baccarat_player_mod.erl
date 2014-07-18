-module(baccarat_player_mod).
-export([payout/2]).

payout(Cards=#{},commission) when map_size(Cards) >= 4 ->
	baccarat_payout_commission:payout(Cards);
payout(Cards=#{},nocommission) when map_size(Cards) >=4 ->
	baccarat_payout_nocommission:payout(Cards).