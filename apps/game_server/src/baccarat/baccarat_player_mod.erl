-module(baccarat_player_mod).
-export([payout/2,try_bet/3,player_quit/3]).

-define(GAME_API,baccarat_game_api).

player_quit(Server,User,Reason)->
	?GAME_API:player_quit(Server,User,Reason).

try_bet(Server,Cats,Amounts)->
	?GAME_API:try_bet(Server,Cats,Amounts).

payout(Cards=#{},commission) when map_size(Cards) >= 4 ->
	baccarat_payout_commission:payout(Cards);
payout(Cards=#{},nocommission) when map_size(Cards) >=4 ->
	baccarat_payout_nocommission:payout(Cards).