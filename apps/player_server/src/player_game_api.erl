-module(player_game_api).
-export([find_server/1,check_bets/3]).

check_bets(Cats,Amounts,baccarat)->
	player_baccarat:check_bets(Cats,Amounts);
check_bets(Cats,Amounts,dragontiger)->
	player_dragontiger:check_bets(Cats,Amounts).

find_server(Table)->
	global:whereis_name({game_server,Table}).