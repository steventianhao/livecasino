-module(player_game_api).
-export([find_server/1,check_bets/3,bet/4]).

check_bets(Cats,Amounts,baccarat)->
	player_baccarat:check_bets(Cats,Amounts);
check_bets(Cats,Amounts,dragontiger)->
	player_dragontiger:check_bets(Cats,Amounts).

find_server(Table)->
	global:whereis_name({game_server,Table}).

bet(Pid,Game,Cats,Amounts)->
	case check_bets(Cats,Amounts,Game) of
		true->
			gen_server:call(Pid,{bet,Cats,Amounts});
		false->
			{error,invalid_bets}
	end.