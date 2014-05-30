-module(dragontiger_payout_dragontiger).
-export([ratio/1]).

-include("dragontiger.hrl").

ratio(dragon)-> 
	{?BET_DRAGON,2};
ratio(tiger)-> 
	{?BET_TIGER,2};
ratio(tie)->
	{?BET_TIE,9};
ratio(dragon_odd)->
	{?BET_DRAGON_ODD,2};
ratio(tiger_odd)->
	{?BET_TIGER_ODD,2};
ratio(dragon_even)->
	{?BET_DRAGON_EVEN,2};
ratio(tiger_even)->
	{?BET_TIGER_EVEN,2};
ratio(tiger_tie)-> 
	{?BET_TIGER,0.5};
ratio(dragon_tie) ->
	{?BET_DRAGON,0.5}.