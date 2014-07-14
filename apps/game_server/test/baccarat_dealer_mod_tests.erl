-module(baccarat_dealer_mod_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/baccarat/baccarat.hrl").
-include("card.hrl").
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
	"SQC8H3#D7D3C9","CKDKC4#CKHAST","S3DACT#SKS2D8","S5DTCQ#S4H5D7","S3DACT#SKS2D8","S8S4ST#S5D6S6","H5H5S6#STSQST",
	"SQSJH2#S2D7D3","H9S4CJ#H6DKHJ","S4SKS5#D9H9D4","D8H4H7#HTH5H7","C2HTC5#C8D5S8","SJD7D3#S6DTD3","CADQCT#SJC5C5",
	"HQC6S4#S2CQH3","DKSACT#C2HQD2","DJC7C8#HQS3DT","D2CKST#D9CJHJ","H6HAC4#C4SQHT","S8S7C5#C9SQDT","D3HQD5#D9SADT",
	"C7C2H2#HKH7S3","H9H4HA#D3STD2","S9CTDT#S9S2DQ","DACTDA#CJCAHA","S5CJC3#D6CKCQ","C4HJC3#CKC2S8","DKH2S9#HKS8S4",
	"S3C4HT#DJD2CK","S4C3D8#HTDJST","S4H9S2#D6SKCT","S5D6D7#H9C4H8","S5D3HT#H6STSA","S7SKH3#C2H3DQ","C6S7S3#SACTS2",
	"H9C8H4#H7C9D2","H6STDT#S2SQD6","D6C3DA#C5C2DQ","HKH7#SASKSQ","D9H3C2#CKS6","H4H7S5#C6DT","H9DQS3#DJH4","H4C3#C9H5SK",
	"CQD6#CKH8D2","S5D6D5#S7CK","C8DJHK#H5D9","S5DKSK#HQS7","S4STS2#HAC6","HAD5S6#S8D6","C3HACA#C6DT","DJH2S8#CTC7",
	"C7DJ#C2CADQ","HJH4S8#CQC6","C2C2HT#C6ST","C2S5#DQSKCT","S6DJ#D5CJHJ","S9D2HJ#D3C2","H9H9S4#D7CT","C8C6C8#H4CT",
	"S2H8S2#D2C3","D5SKSQ#DJC6","DKS7#HJH3SQ","H6DA#S7C9H5","S4DAHK#C7SK","CQHADK#H6C9","HJCQDA#D8C8","SQC5ST#SQD4",
	"S7CQ#CTDQSJ","C7DT#HASKS3","D8HK#D9HT","C7S2#SJC6","C3C5#S9SJ","D9C9#D5S5","HJC8#STH6","SJH7#D2D4","HQC9#DQS6",
	"S6SJ#D7HK","SAD8#DTD4","D9SJ#H2C9","S8SA#HAD6","DTDA#H9HK","H8S5#C9CJ","S5C4#HQD2","S7SA#D2SA","DKC8#C2HT",
	"STS9#D7S3","DQS8#C6D5","H9HK#CQS2","DTC9#C2D5","DAC8#D7S8","S3D3#D8HA","S9DK#H3CQ","D2S3#DAC8","H3C8#S4D5",
	"D9C9#S2D6","H5SA#D7HK","HQH7#CQD9","S3SA#DJS9","D9S7#H4C5","HKS7#H2D6","DJH8#CJH6","H7D8#DKS8","H8ST#H8H5"].

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
	L1=[#card{rank=?ACE},#card{rank=?TWO},#card{rank=?EIGHT},#card{rank=?THREE},#card{rank=?FOUR}],
	L2=[#card{rank=?ACE},#card{rank=?TWO},#card{rank=?SEVEN},#card{rank=?THREE},#card{rank=?FOUR}],
	L3=[#card{rank=?TWO},#card{rank=?ACE},#card{rank=?THREE},#card{rank=?EIGHT},#card{rank=?FOUR}],
	L4=[#card{rank=?TWO},#card{rank=?ACE},#card{rank=?THREE},#card{rank=?SEVEN},#card{rank=?FOUR}],
	L5=[#card{rank=?ACE},#card{rank=?TWO},#card{rank=?FIVE},#card{rank=?FOUR},#card{rank=?SIX}],
	L6=[#card{rank=?ACE},#card{rank=?TWO},#card{rank=?FIVE},#card{rank=?FIVE},#card{rank=?SIX}],
	L7=[#card{rank=?ACE},#card{rank=?TWO},#card{rank=?SIX},#card{rank=?FOUR},#card{rank=?EIGHT}],
	L8=[#card{rank=?ACE},#card{rank=?TWO},#card{rank=?SIX},#card{rank=?FIVE},#card{rank=?EIGHT}],
	lists:map(fun(L)->?_assert(four_add_invalid_pos(L)) end,
		[L1,L2,L3,L4,L5,L6,L7,L8]).	

put_test_()->
	lists:map(fun(L)-> ?_assert(put(L)) end, string_cases()).

put_test()->
	{ok,C1}=baccarat_dealer_mod:put(?PLAYER_POS_1,#card{rank=?TWO},#{}),
	?assert(C1=:=#{?PLAYER_POS_1=>#card{rank=?TWO}}),
	{ok,C2}=baccarat_dealer_mod:put(?PLAYER_POS_1,#card{rank=?TWO},C1),
	?assert(C2=:=#{?PLAYER_POS_1=>#card{rank=?TWO}}),
	{ok,C3}=baccarat_dealer_mod:put(?PLAYER_POS_2,#card{rank=?TWO},C2),
	?assert(C3=:=#{?PLAYER_POS_1=>#card{rank=?TWO},?PLAYER_POS_2=>#card{rank=?TWO}}),
	?assert(error=:=baccarat_dealer_mod:put(-1,#card{rank=?ACE},#{})).

validate_test_()->
	List1=[{?PLAYER_POS_1,#card{rank=?JACK}},{?PLAYER_POS_2,#card{rank=?FOUR}},
	{?BANKER_POS_2,#card{rank=?TEN}},{?PLAYER_POS_3,#card{rank=?NINE}},{?BANKER_POS_3,#card{rank=?THREE}}],
	Result1=baccarat_dealer_mod:validate(maps:from_list(List1)),
	List2=[{?PLAYER_POS_1,#card{rank=?JACK}}],
	Result2=baccarat_dealer_mod:validate(maps:from_list(List2)),
	[?_assertNot(Result1),?_assertNot(Result2)].

remove_test()->
	M=#{?PLAYER_POS_1=>#card{rank=?JACK},?PLAYER_POS_2=>#card{rank=?KING}},
	?assert({ok,#{?PLAYER_POS_2=>#card{rank=?KING}}}=:=baccarat_dealer_mod:remove(?PLAYER_POS_1,M)),
	?assert(error=:=baccarat_dealer_mod:remove(?BANKER_POS_2,M)).

validate_7cards_test()->
	C1="S3DACT#SKS2D8",
	Cards=baccarat_dealer_mod:from_string(C1),
	C2=maps:merge(#{7 =>casino_card:one_card(<<"H2">>)},Cards),
	?assertEqual(false,baccarat_dealer_mod:validate(C2)).

validate_3cards_test()->
	C1="S3#SKS2D8",
	Cards=baccarat_dealer_mod:from_string(C1),
	%C2=maps:merge(#{7 =>baccarat_dealer_mod:one_card("H2")},Cards),
	?assertEqual(false,baccarat_dealer_mod:validate(Cards)).