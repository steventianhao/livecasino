-module(dragontiger_player_mod).
-export([payout/2,player_quit/3,try_bet/3]).

-define(GAME_API,dragontiger_game_api).

player_quit(Server,User,Reason)->
	?GAME_API:player_quit(Server,User,Reason).

try_bet(Server,Cats,Amounts)->
	?GAME_API:try_bet(Server,Cats,Amounts).

payout(Cards,dragontiger)->
	dragontiger_payout:payout(Cards).
	