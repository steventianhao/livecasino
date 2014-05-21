-module(baccarat_player_mod).

-record(card,{suit,name,value}).


-define(BET_TYPES,#{
	1000 => banker,
	1001 => player,
	1002 => tie,
	1003 => banker_pair,
	1004 => player_pair,
	1005 => banker_n8,
	1006 => banker_n9,
	1007 => player_n8,
	1008 => player_n9,
	1009 => big,
	1010 => small
	}).

-define(ResultMap,#{banker=>false,
	player=>false,
	tie=>false,
	banker_pair=>false,
	player_pair=>false,
	big=>false,
	small=>false,
	banker_n8=>false,
	player_n8=>false,
	banker_n9=>false,
	player_n9=>false}).


-record(info,{banker_total,player_total,banker_cards_length,player_cards_length,banker_pair=false,player_pair=false}).


total(Cards) ->
	lists:foldl(fun(X,Sum)->X#card.value+Sum end,0,Cards) rem 10.

pair(#card{name=N},#card{name=N})->
	true;
pair(Card1,Card2) when is_record(Card1,card) andalso is_record(Card2,card)->
	false.

n8n9(true,2,8)-> 
	{true,fale};
n8n9(true,2,9)-> 
	{false,true};
n8n9(_,_,_)->
	{false,false}.

bigsmall(L) when L > 4->
	{true,false};
bigsmall(L) when L ==4 ->
	{false,true}.

summary(PlayerCards=[P1,P2|_],BankerCards=[B1,B2|_])->
	Bt=total(BankerCards),
	Bl=length(BankerCards),
	Bp=pair(B1,B2),
	Pt=total(PlayerCards),
	Pl=length(PlayerCards),
	Pp=pair(P1,P2),
	#info{banker_total=Bt,player_total=Pt,banker_cards_length=Bl,player_cards_length=Pl,banker_pair=Bp,player_pair=Pp}.

		
%%Sum=summary(PlayerCards,BankerCards),
result(#info{banker_total=Bt,player_total=Pt,banker_cards_length=Bl,player_cards_length=Pl,banker_pair=Bp,player_pair=Pp},WithBigSmall) ->
	IsBanker=Bt > Pt,
	IsPlayer=Bt < Pt,
	IsTie = Bt == Pt,
	IsBankerPair = Bp,
	IsPlayerPair= Pp,
	{IsBankerN8,IsBankerN9}=n8n9(IsBanker,Bl,Bt),
	{IsPlayerN8,IsPlayerN9}=n8n9(IsPlayer,Pl,Pt),
	case WithBigSmall of 
		true->
			{IsBig,IsSmall}=bigsmall(Bl+Pl),
			{IsBanker,IsPlayer,IsTie,IsBankerPair,IsPlayerPair,IsBankerN8,IsPlayerN8,IsBankerN9,IsPlayerN9,IsBig,IsSmall};
		_ ->
			{IsBanker,IsPlayer,IsTie,IsBankerPair,IsPlayerPair,IsBankerN8,IsPlayerN8,IsBankerN9,IsPlayerN9,false,false}
	end.
