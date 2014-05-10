-module(baccarat_dealer_mod_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/baccarat.hrl").
-define(DEBUG,true).

put(List)->
	Fun = fun({Pos,Card},Acc)-> 
		baccarat_dealer_mod:put(Pos,Card,Acc) 
	end,
	Cards=lists:foldl(Fun, #{},List),
	baccarat_dealer_mod:commit(Cards).

add_test() ->
	Fun = fun(Card,Acc)-> 
		{_,Cards}=baccarat_dealer_mod:add(Card,Acc),
		Cards 
	end,
	List = [?SIX,?FIVE,?SEVEN,?THREE],
	M=lists:foldl(Fun, #{},List),
	?debugVal(M),
	Result=[{?PLAYER_POS_1,?SIX},{?PLAYER_POS_2,?SEVEN},{?BANKER_POS_1,?FIVE},{?BANKER_POS_2,?THREE}],
	?debugVal(Result),
	?assert(M == maps:from_list(Result)).

put_test_()->
	List1=[{?PLAYER_POS_1,?SIX},{?PLAYER_POS_2,?SEVEN},{?BANKER_POS_1,?FIVE},{?BANKER_POS_2,?THREE}],
	List2=[{?PLAYER_POS_1,?JACK},{?PLAYER_POS_2,?FOUR},{?PLAYER_POS_3,?NINE},{?BANKER_POS_1,?THREE},{?BANKER_POS_2,?TEN},{?BANKER_POS_3,?THREE}],
	List3=[{?PLAYER_POS_1,?TWO},{?PLAYER_POS_2,?NINE},{?PLAYER_POS_3,?ACE},{?BANKER_POS_1,?FIVE},{?BANKER_POS_2,?ACE}],
	List4=[{?PLAYER_POS_1,?EIGHT},{?PLAYER_POS_2,?QUEEN},{?BANKER_POS_1,?SIX},{?BANKER_POS_2,?JACK}],
	List5=[{?PLAYER_POS_1,?ACE},{?PLAYER_POS_2,?SIX},{?BANKER_POS_1,?TWO},{?BANKER_POS_2,?SIX}],
	List6=[{?PLAYER_POS_1,?NINE},{?PLAYER_POS_2,?SIX},{?PLAYER_POS_3,?EIGHT},{?BANKER_POS_1,?KING},{?BANKER_POS_2,?THREE}],
	List7=[{?PLAYER_POS_1,?THREE},{?PLAYER_POS_2,?FOUR},{?BANKER_POS_1,?SIX},{?BANKER_POS_2,?FIVE},{?BANKER_POS_3,?TWO}],
	List8=[{?PLAYER_POS_1,?EIGHT},{?PLAYER_POS_2,?KING},{?BANKER_POS_1,?FOUR},{?BANKER_POS_2,?FOUR}],
	List9=[{?PLAYER_POS_1,?TEN},{?PLAYER_POS_2,?QUEEN},{?PLAYER_POS_3,?TEN},{?BANKER_POS_1,?EIGHT},{?BANKER_POS_2,?EIGHT}],
	All=[List1,List2,List3,List4,List5,List6,List7,List8,List9],
	lists:map(fun(L)-> ?_assert(put(L)) end, All).