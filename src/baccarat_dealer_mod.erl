-module(baccarat_dealer_mod).

-export([add/2,put/3,remove/2,validate/1,create/1]).
-include("baccarat.hrl").

-define(ANYONEOF(Total,Lists),lists:member(Total,Lists)).

create(Cards)->
	[Pcs,Bcs]=string:tokens(Cards,"#"),
	CardsMap=?CARDS_MAP,
	maps:merge(create_player_cards(Pcs,CardsMap),create_banker_cards(Bcs,CardsMap)).
create_player_cards(Cards,CardsMap)->
	CL=lists:reverse(string_to_cards(Cards,CardsMap)),
	create_map(CL,[?PLAYER_POS_1,?PLAYER_POS_2,?PLAYER_POS_3]).
create_banker_cards(Cards,CardsMap)->
	CL=lists:reverse(string_to_cards(Cards,CardsMap)),
	create_map(CL,[?BANKER_POS_1,?BANKER_POS_2,?BANKER_POS_3]).
create_map(Cards,IndexList)->
	case {Cards,IndexList} of
		{[],_} -> #{};
		{[H1|T1],[H2|T2]} -> 
			Rest=create_map(T1,T2),
			maps:put(H2,H1,Rest)
	end.

string_to_cards([],_CardsMap)->
	[];
string_to_cards([_,N|T],CardsMap)->
	C= maps:get(N,CardsMap),
	[C|string_to_cards(T,CardsMap)].


total(Cards) ->
	lists:foldl(fun(X,Sum)->X#card.value+Sum end,0,Cards) rem 10.

put(Pos,Card,Cards) when is_map(Cards) andalso is_record(Card,card) andalso is_integer(Pos)->
	case lists:member(Pos,?ALL_POS) of
		true->
			{ok,maps:put(Pos,Card,Cards)};
		false->
			error
	end.

remove(Pos,Cards) when is_map(Cards) andalso is_integer(Pos)->
	case maps:is_key(Pos,Cards) of
		true -> 
			{ok,maps:remove(Pos,Cards)};
		false->
			error
	end.

validate(Cards=#{?PLAYER_POS_1 :=P1,?PLAYER_POS_2 :=P2,?BANKER_POS_1 :=B1,?BANKER_POS_2 :=B2}) when is_map(Cards)->
	Size = maps:size(Cards),
	Bt=total([B1,B2]),
	Pt=total([P1,P2]),
	case {Size,Cards} of
		{4,_}->
			?ANYONEOF(Pt,?TOTAL89) orelse ?ANYONEOF(Bt,?TOTAL89) orelse (?ANYONEOF(Pt,?TOTAL67) andalso ?ANYONEOF(Bt,?TOTAL67));
		{5,#{?PLAYER_POS_3 :=P3}}->
			P3v=P3#card.value,
			Bt3=(Bt==3 andalso P3v ==8)  ,
			Bt4=(Bt==4 andalso ?ANYONEOF(P3v,[0,1,8,9])),
			Bt5=(Bt==5 andalso (not ?ANYONEOF(P3v,[4,5,6,7]))),
			Bt6=(Bt==6 andalso (not ?ANYONEOF(P3v,?TOTAL67))),
			Pt < 6 andalso  (Bt3 orelse Bt4 orelse Bt5 orelse Bt6 orelse Bt==7);
		{5,#{?BANKER_POS_3 :=_}}->
			?ANYONEOF(Pt,?TOTAL67) andalso Bt < 6;
		{6,#{?PLAYER_POS_3 :=P3,?BANKER_POS_3 :=_}}->
			P3v=P3#card.value,
			Bt3=(Bt==3 andalso P3v /=8) ,
			Bt4=(Bt==4 andalso (not ?ANYONEOF(P3v,[0,1,8,9]))),
			Bt5=(Bt==5 andalso ?ANYONEOF(P3v,[4,5,6,7])),
			Bt6=(Bt==6 andalso ?ANYONEOF(P3v,?TOTAL67)),
			Pt < 6 andalso (Bt <3 orelse Bt3 orelse Bt4 orelse Bt5 orelse Bt6);
		_ -> false
	end;
validate(Cards) when is_map(Cards)->
	false.


add(Card,Cards) when is_map(Cards) andalso is_record(Card,card)->
	Calc4 = fun(Pt,Bt)->
		if
			Pt == 8 orelse Pt ==9 orelse Bt==8 orelse Bt==9 -> ?INVALID_POS;
			(Pt== 6 orelse Pt == 7) andalso (Bt == 6 orelse Bt==7) -> ?INVALID_POS;
			(Pt== 6 orelse Pt == 7) andalso Bt < 6 -> ?BANKER_POS_3;
			true -> ?PLAYER_POS_3
		end
	end,

	Calc5 = fun(Bt,P3v)->
		case Bt of
			T  when T< 3 -> ?BANKER_POS_3;
			3 when  P3v /= 8 -> ?BANKER_POS_3;
			4 when P3v /= 8 orelse P3v /= 9 orelse P3v /=1 orelse P3v /=0 -> ?BANKER_POS_3;
			5 when P3v == 4 orelse P3v == 5 orelse P3v ==6 orelse P3v ==7 -> ?BANKER_POS_3;
			6 when P3v == 6 orelse P3v == 7 -> ?BANKER_POS_3;
			_ ->?INVALID_POS
		end
	end,

	Pos=case {maps:size(Cards),Cards} of
		{0,_}-> 
			?PLAYER_POS_1;
		{1,#{?PLAYER_POS_1 := _}}->
			?BANKER_POS_1;
		{2,#{?PLAYER_POS_1 := _, ?BANKER_POS_1 := _}}->
			?PLAYER_POS_2;
		{3,#{?PLAYER_POS_1 := _, ?BANKER_POS_1 := _, ?PLAYER_POS_2 :=_}}->
			?BANKER_POS_2;
		{4,#{?PLAYER_POS_1 :=P1, ?BANKER_POS_1 :=B1,?PLAYER_POS_2 :=P2,?BANKER_POS_2 := B2}}->
			Calc4(total([P1,P2]),total([B1,B2]));
		{5,#{?PLAYER_POS_1:=_,?PLAYER_POS_2:=_,?PLAYER_POS_3:=P3,?BANKER_POS_1:=B1,?BANKER_POS_2:=B2}}->
			Calc5(total([B1,B2]),P3#card.value);
		_ -> 
			?INVALID_POS
	end,
	case Pos of
		?INVALID_POS -> {Pos,Cards};
		_ -> {Pos,maps:put(Pos,Card,Cards)}
	end.