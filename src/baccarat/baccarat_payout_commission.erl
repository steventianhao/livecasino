-module(baccarat_payout_commission).
-export([ratio/1,reward/1,payout/1]).
-include("baccarat.hrl").

ratio(banker)->
	{?BET_BANKER,1.95};
ratio(player)-> 
	{?BET_PLAYER,2};
ratio(tie) -> 
	{?BET_TIE,9};
ratio(banker_pair) -> 
	{?BET_BANKER_PAIR,12};
ratio(player_pair) ->
	{?BET_PLAYER_PAIR,12};
ratio(banker_n8)->
	{?BET_BANKER_N8,9};
ratio(banker_n9)->
	{?BET_BANKER_N9,9};
ratio(player_n8)->
	{?BET_PLAYER_N8,9};
ratio(player_n9)->
	{?BET_PLAYER_N9,9};
ratio(big)->
	{?BET_BIG,1.53};
ratio(small)->
	{?BET_SMALL,2.45};
	
ratio(banker_tie)->
	{?BET_BANKER,1};
ratio(player_tie) ->
	{?BET_PLAYER,1}.

result4plus(Pt,Bt) when Pt==Bt ->
	[tie,banker_tie,player_tie];
result4plus(Pt,Bt) when Pt>Bt ->
	[player];
result4plus(Pt,Bt) when Pt<Bt ->
	[banker].

reward(Cards) when is_map(Cards) andalso map_size(Cards)==4 ->
	baccarat_payout:reward4(Cards);
reward(Cards) when is_map(Cards) andalso map_size(Cards)>4 ->
	baccarat_payout:reward4plus(Cards,fun result4plus/2).

payout(Cards=#{})->
	casino_bets:payout(Cards,fun ratio/1,fun reward/1).