-module(baccarat_dealer_mod_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/baccarat/baccarat.hrl").
-define(DEBUG,true).

put(List)->
	Fun = fun({Pos,Card},Acc)-> 
		{ok,Acc2}=baccarat_dealer_mod:put(Pos,Card,Acc),
		Acc2
	end,
	Cards=lists:foldl(Fun, #{},List),
	baccarat_dealer_mod:validate(Cards).

add(List)->
	add2([Card || {_,Card} <- List]).

add2(List)->
	Fun = fun(Card,Acc)-> 
		case baccarat_dealer_mod:add(Card,Acc) of
			{error,Cards} ->
				Cards;
			{_Status,_Pos,Cards}->
				Cards
		end
	end,
	lists:foldl(Fun, #{},List).

results()->
	["HQD7#HAH5C5","S7D7#H9HK","CTD7#HJH8","C4D4D7#H6ST","D9D7#CTHKD2","CTD7#HKC4DJ","DAD7#H4S3","DJD7#DKS9",
	"HKD6D7#S3H3SK","DAS4D7#C2H2","SAD7#S3C5","D4S7D7#C9SKHA","D2C8D7#H8C2DQ","HAD7#C2H9","HAS7D7#H6DAH2",
	"DKD7#H8D3SJ","S9D8D7#H4H2C9","S9D7#DQSQDT","H2DTD2#DKSQCJ","D4C3D8#DTC2H8","S2C3C9#S9DAD9","DQDKSQ#D5D2DT",
	"D5HQHT#H9C6D4","C9C2CT#H9C8C2","S6SQST#H6STC6","H7CQCT#STSQHJ","C3HQS5#D2SQSJ","S5HQSJ#C4SAHK","DJH3SJ#D3DADJ",
	"H6DKDA#CKSQCT","D4C4H7#DQDKDQ","S2CJS5#DTHTDJ","C2HJCJ#CQHKD2","C4D7C5#D6DASA","HQHAHQ#S3S3SJ",
	"SQC8H3#D7D3C9","CKDKC4#CKHAST","S3DACT#SKS2D8","S5DTCQ#S4H5D7","S3DACT#SKS2D8"].

string_cases()->
	lists:map(fun(L)->maps:to_list(baccarat_dealer_mod:from_string(L)) end,results()).


from_string_test()->
	Result=lists:all(fun(Cards)-> Cards== baccarat_dealer_mod:to_string(baccarat_dealer_mod:from_string(Cards)) end,results()),
	?assert(Result).

add_test_() ->
	lists:map(fun(L)-> ?_assert(add(L) == maps:from_list(L)) end, string_cases()).

four_add_invalid_pos(L)->
	M=add2(L),
	maps:size(M)==4 andalso maps:is_key(?BANKER_POS_3,M) ==false andalso maps:is_key(?PLAYER_POS_3,M) ==false.


add2_test_()->
	L1=[?ACE,?TWO,?EIGHT,?THREE,?FOUR],
	L2=[?ACE,?TWO,?SEVEN,?THREE,?FOUR],
	L3=[?TWO,?ACE,?THREE,?EIGHT,?FOUR],
	L4=[?TWO,?ACE,?THREE,?SEVEN,?FOUR],
	L5=[?ACE,?TWO,?FIVE,?FOUR,?SIX],
	L6=[?ACE,?TWO,?FIVE,?FIVE,?SIX],
	L7=[?ACE,?TWO,?SIX,?FOUR,?EIGHT],
	L8=[?ACE,?TWO,?SIX,?FIVE,?EIGHT],
	lists:map(fun(L)->?_assert(four_add_invalid_pos(L)) end,
		[L1,L2,L3,L4,L5,L6,L7,L8]).	



put_test_()->
	lists:map(fun(L)-> ?_assert(put(L)) end, string_cases()).

put_test()->
	{ok,C1}=baccarat_dealer_mod:put(?PLAYER_POS_1,?TWO,#{}),
	?assert(C1=:=#{?PLAYER_POS_1=>?TWO}),
	{ok,C2}=baccarat_dealer_mod:put(?PLAYER_POS_1,?TWO,C1),
	?assert(C2=:=#{?PLAYER_POS_1=>?TWO}),
	{ok,C3}=baccarat_dealer_mod:put(?PLAYER_POS_2,?TWO,C2),
	?assert(C3=:=#{?PLAYER_POS_1=>?TWO,?PLAYER_POS_2=>?TWO}),
	?assert(error=:=baccarat_dealer_mod:put(-1,?ACE,#{})).

validate_test_()->
	List1=[{?PLAYER_POS_1,?JACK},{?PLAYER_POS_2,?FOUR},{?BANKER_POS_2,?TEN},{?PLAYER_POS_3,?NINE},{?BANKER_POS_3,?THREE}],
	Result1=baccarat_dealer_mod:validate(maps:from_list(List1)),
	List2=[{?PLAYER_POS_1,?JACK}],
	Result2=baccarat_dealer_mod:validate(maps:from_list(List2)),
	[?_assertNot(Result1),?_assertNot(Result2)].

remove_test()->
	M=#{?PLAYER_POS_1=>?JACK,?PLAYER_POS_2=>?KING},
	?assert({ok,#{?PLAYER_POS_2=>?KING}}=:=baccarat_dealer_mod:remove(?PLAYER_POS_1,M)),
	?assert(error=:=baccarat_dealer_mod:remove(?BANKER_POS_2,M)).

