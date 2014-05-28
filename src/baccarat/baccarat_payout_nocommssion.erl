-module(baccarat_payout_nocommssion).

-export([ratio/1,rewards/1]).

ratio(banker)->
	{1100,2};
ratio(player)-> 
	{1101,2};
ratio(tie) -> 
	{1102,9};
ratio(banker_pair) -> 
	{1103,12};
ratio(player_pair) ->
	{1104,12};
ratio(banker_n8)->
	{1105,9};
ratio(banker_n9)->
	{1106,9};
ratio(player_n8)->
	{1107,9};
ratio(player_n9)->
	{1108,9};
ratio(big)->
	{1109,1.53};
ratio(small)->
	{1110,2.45};

ratio(banker_tie)->
	{1111,1};
ratio(player_tie) ->
	{1112,1};

ratio(banker6)->
	{1113,1.5}.


rewards(banker)->
	[banker,banker_tie,banker6];
rewards(player)->
	[player,player_tie];
rewards(tie)->
	[tie];
rewards(banker_pair)->
	[banker_pair];
rewards(player_pair)->
	[player_pair];
rewards(banker_n8)->
	[banker_n8];
rewards(banker_n9)->
	[banker_n9];
rewards(player_n8)->
	[player_n8];
rewards(player_n9)->
	[player_n9];
rewards(big)->
	[big];
rewards(small)->
	[small].