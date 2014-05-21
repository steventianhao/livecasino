-module(baccarat_payout_nocommssion).

-export([ratio/1,rewards/1]).

ratio(banker)->
	{2000,2};
ratio(player)-> 
	{2001,2};
ratio(tie) -> 
	{2002,9};
ratio(banker_pair) -> 
	{2003,12};
ratio(player_pair) ->
	{2004,12};
ratio(banker_n8)->
	{2005,9};
ratio(banker_n9)->
	{2006,9};
ratio(player_n8)->
	{2007,9};
ratio(player_n9)->
	{2008,9};
ratio(big)->
	{2009,0.53};
ratio(small)->
	{2010,1.45};

ratio(banker_tie)->
	{2011,1};
ratio(player_tie) ->
	{2012,1};

ratio(banker6)->
	{2013,1.5}.


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