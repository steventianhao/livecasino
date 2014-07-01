-module(baccarat_payout_nocommission).
-include("baccarat.hrl").
-export([ratio/1,reward/1,payout/1]).

ratio(banker)->
	{?BET_BANKER,2,?BET_BANKER};
ratio(banker6)->
	{?BET_BANKER,1.5,?BET_BANKER};
ratio(player)-> 
	{?BET_PLAYER,2,?BET_PLAYER};
ratio(tie) -> 
	{?BET_TIE,9,?BET_TIE};
ratio(banker_tie)->
	{?BET_BANKER,1,?BET_TIE};
ratio(player_tie) ->
	{?BET_PLAYER,1,?BET_TIE};
ratio(banker_pair) -> 
	{?BET_BANKER_PAIR,12,?BET_BANKER_PAIR};
ratio(player_pair) ->
	{?BET_PLAYER_PAIR,12,?BET_PLAYER_PAIR};
ratio(banker_n8)->
	{?BET_BANKER_N8,9,?BET_BANKER_N8};
ratio(banker_n9)->
	{?BET_BANKER_N9,9,?BET_BANKER_N9};
ratio(player_n8)->
	{?BET_PLAYER_N8,9,?BET_PLAYER_N8};
ratio(player_n9)->
	{?BET_PLAYER_N9,9,?BET_PLAYER_N9};
ratio(big)->
	{?BET_BIG,1.53,?BET_BIG};
ratio(small)->
	{?BET_SMALL,2.45,?BET_SMALL}.

result4plus(Pt,Bt) when Pt==Bt ->
	[tie,banker_tie,player_tie];
result4plus(Pt,Bt) when Pt>Bt ->
	[player];
result4plus(Pt,Bt=6) when Pt<Bt ->
	[banker6];
result4plus(Pt,Bt) when Pt<Bt ->
	[banker].

reward(Cards) when is_map(Cards) andalso map_size(Cards)==4 ->
	baccarat_payout:reward4(Cards);
reward(Cards) when is_map(Cards) andalso map_size(Cards)>4 ->
	baccarat_payout:reward4plus(Cards,fun result4plus/2).

result(Cards)->
	Fun=fun(E)-> {_,_,B}=ratio(E),B end,
	lists:map(Fun,reward(Cards)).
	
payout(Cards=#{})->
	Fun=fun(E)->{B,R,_}=ratio(E),{B,R} end,
	maps:from_list(lists:map(Fun,reward(Cards))).