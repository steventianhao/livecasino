-module(baccarat_dealer_mod_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/baccarat.hrl").
-define(DEBUG,true).

put(List)->
	Fun = fun({Pos,Card},Acc)-> 
		{ok,Acc2}=baccarat_dealer_mod:put(Pos,Card,Acc),
		Acc2
	end,
	Cards=lists:foldl(Fun, #{},List),
	baccarat_dealer_mod:commit(Cards).

add(List)->
	Fun = fun(Card,Acc)-> 
		{_,Cards}=baccarat_dealer_mod:add(Card,Acc),
		Cards 
	end,
	L=[Card || {_,Card} <- List],
	lists:foldl(Fun, #{},L).

all_cases()->
	L1=[{?PLAYER_POS_1,?SIX}, {?BANKER_POS_1,?FIVE},{?PLAYER_POS_2,?SEVEN},{?BANKER_POS_2,?THREE}],
	L2=[{?PLAYER_POS_1,?JACK},{?BANKER_POS_1,?THREE},{?PLAYER_POS_2,?FOUR},{?BANKER_POS_2,?TEN},{?PLAYER_POS_3,?NINE},{?BANKER_POS_3,?THREE}],
	L3=[{?PLAYER_POS_1,?TWO}, {?BANKER_POS_1,?FIVE},{?PLAYER_POS_2,?NINE},{?BANKER_POS_2,?ACE},{?PLAYER_POS_3,?ACE}],
	L4=[{?PLAYER_POS_1,?EIGHT},{?BANKER_POS_1,?SIX},{?PLAYER_POS_2,?QUEEN},{?BANKER_POS_2,?JACK}],
	L5=[{?PLAYER_POS_1,?ACE},{?BANKER_POS_1,?TWO}, {?PLAYER_POS_2,?SIX},{?BANKER_POS_2,?SIX}],
	L6=[{?PLAYER_POS_1,?NINE},{?BANKER_POS_1,?KING},{?PLAYER_POS_2,?SIX},{?BANKER_POS_2,?THREE},{?PLAYER_POS_3,?EIGHT}],
	L7=[{?PLAYER_POS_1,?THREE},{?BANKER_POS_1,?SIX},{?PLAYER_POS_2,?FOUR},{?BANKER_POS_2,?FIVE},{?BANKER_POS_3,?TWO}],
	L8=[{?PLAYER_POS_1,?EIGHT},{?BANKER_POS_1,?FOUR},{?PLAYER_POS_2,?KING},{?BANKER_POS_2,?FOUR}],
	L9=[{?PLAYER_POS_1,?TEN},{?BANKER_POS_1,?EIGHT},{?PLAYER_POS_2,?QUEEN},{?BANKER_POS_2,?EIGHT},{?PLAYER_POS_3,?TEN}],
	[L1,L2,L3,L4,L5,L6,L7,L8,L9].

add_test_() ->
	lists:map(fun(L)-> ?_assert(add(L) == maps:from_list(L)) end, all_cases()).

put_test_()->
	lists:map(fun(L)-> ?_assert(put(L)) end, all_cases()).

put_test()->
	{ok,C1}=baccarat_dealer_mod:put(?PLAYER_POS_1,?TWO,#{}),
	?assert(C1=:=#{?PLAYER_POS_1=>?TWO}),
	{ok,C2}=baccarat_dealer_mod:put(?PLAYER_POS_1,?TWO,C1),
	?assert(C2=:=#{?PLAYER_POS_1=>?TWO}),
	{ok,C3}=baccarat_dealer_mod:put(?PLAYER_POS_2,?TWO,C2),
	?assert(C3=:=#{?PLAYER_POS_1=>?TWO,?PLAYER_POS_2=>?TWO}),
	?assert(error=:=baccarat_dealer_mod:put(?INVALID_POS,?ACE,#{})).

commit_test_()->
	List1=[{?PLAYER_POS_1,?JACK},{?PLAYER_POS_2,?FOUR},{?BANKER_POS_2,?TEN},{?PLAYER_POS_3,?NINE},{?BANKER_POS_3,?THREE}],
	Result1=baccarat_dealer_mod:commit(maps:from_list(List1)),
	List2=[{?PLAYER_POS_1,?JACK}],
	Result2=baccarat_dealer_mod:commit(maps:from_list(List2)),
	[?_assertNot(Result1),?_assertNot(Result2)].

remove_test()->
	M=#{?PLAYER_POS_1=>?JACK,?PLAYER_POS_2=>?KING},
	?assert({ok,#{?PLAYER_POS_2=>?KING}}=:=baccarat_dealer_mod:remove(?PLAYER_POS_1,M)),
	?assert(error=:=baccarat_dealer_mod:remove(?BANKER_POS_2,M)).

