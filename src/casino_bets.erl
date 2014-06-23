-module(casino_bets).

-export([is_valid_bets/3,insert_bets/4,payout_bets/2,payout_bundles/1,payout_total/1]).
-export([create_bet_req/5,create_payout_req/5]).
-export([add_reward/3,baccarat_total/1,payout/3]).

-include("db.hrl").
-include("card.hrl").

is_valid_bet_cats(Cats,AllBetCats)->
	Cs=sets:from_list(Cats),
	L= sets:size(Cs),
	case length(Cats) of
		L->
			sets:is_subset(Cs,sets:from_list(AllBetCats));
		_ ->
			false
	end.
	
is_valid_bet_amounts(Amounts)->
	lists:all(fun(E)-> E>0 end,Amounts).
		
is_valid_bets(Cats=[C1|_],Amounts=[A1|_],AllBetCats=[_|_]) when is_integer(C1) andalso is_number(A1) andalso length(Cats)==length(Amounts)->
	is_valid_bet_cats(Cats,AllBetCats) andalso is_valid_bet_amounts(Amounts);
is_valid_bets(_,_,_)->
	false.

create_bet_req(RoundId,UserId,TableId,Cats,Amounts)->
	Cstr = string:join([integer_to_list(C) || C <-Cats],","),
	Astr = string:join([float_to_list(A,[{decimals,2}]) || A <-Amounts],","),	
	Total = lists:sum(Amounts),
 	#db_bet_req{round_id=RoundId,player_id=UserId,player_table_id=TableId,bet_cats=Cstr,bet_amounts=Astr,total_amount=Total}.

create_payout_req(RoundId,UserId,TableId,Payouts,Total)->
	{Bs,Ps}=lists:unzip(maps:to_list(Payouts)),
	Bstr = string:join([integer_to_list(C) || C <-Bs],","),
	Pstr = string:join([float_to_list(A,[{decimals,2}]) || A <-Ps],","),
	#db_payout_req{round_id=RoundId,player_id=UserId,player_table_id=TableId,bet_bundle_ids=Bstr,payout_amounts=Pstr,total_amount=Total}.	
	

insert_bets(BetEts,BetBundleId,Cats,Amounts)->
	Ts=lists:zipwith(fun(C,A)->{{BetBundleId,C},A,0} end, Cats, Amounts),
	ets:insert(BetEts,Ts).

payout_bet('$end_of_table',_BetEts,_RatioMap)->
	true;
payout_bet(Key={_,Cat},BetEts,RatioMap)->
	case maps:find(Cat,RatioMap) of
		{ok,Ratio}-> 
			true=ets:update_element(BetEts,Key,{3,Ratio});
		error ->
			true
	end,	
	payout_bet(ets:next(BetEts,Key),BetEts,RatioMap).
payout_bets(BetEts,RatioMap)->
	payout_bet(ets:first(BetEts),BetEts,RatioMap).


payout_bundles(BetEts)->
	Fun = fun({{_BetBundleId,_C},_A,0},Acc)->
					Acc;
			 ({{BetBundleId,_C},A,R},Acc)->
			 	case maps:find(BetBundleId,Acc) of
			 		{ok,Total}->
			 			maps:put(BetBundleId,A*R+Total,Acc);
			 		error->
			 			maps:put(BetBundleId,A*R,Acc)
			 	end
	end,
	ets:foldl(Fun,#{},BetEts).

payout_total(PayoutBundles)->
	maps:fold(fun(_K,V,Acc)->Acc+V end,0,PayoutBundles).

payout(Cards=#{},RatioFunc,RewardFunc)->
	maps:from_list(lists:map(RatioFunc,RewardFunc(Cards))).

add_reward(Rewards,Cond,Result) when Cond==true ->
	[Result | Rewards];
add_reward(Rewards,_,_)->
	Rewards.

baccarat_total(Cards) ->
	lists:foldl(fun(X,Sum)->X#card.value+Sum end,0,Cards) rem 10.