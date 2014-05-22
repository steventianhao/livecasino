-module(baccarat_payout_commssion).
-export([ratio/1,rewards/1]).

ratio(banker)->
	{1000,1.95};
ratio(player)-> 
	{1001,2};
ratio(tie) -> 
	{1002,9};
ratio(banker_pair) -> 
	{1003,12};
ratio(player_pair) ->
	{1004,12};
ratio(banker_n8)->
	{1005,9};
ratio(banker_n9)->
	{1006,9};
ratio(player_n8)->
	{1007,9};
ratio(player_n9)->
	{1008,9};
ratio(big)->
	{1009,1.53};
ratio(small)->
	{1010,2.45};

ratio(banker_tie)->
	{1011,1};
ratio(player_tie) ->
	{1012,1}.


rewards(banker)->
	[banker,banker_tie];
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