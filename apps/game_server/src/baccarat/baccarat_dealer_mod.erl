-module(baccarat_dealer_mod).

-export([add/2,put/3,remove/2,validate/1,from_string/1,to_string/1]).
-include("baccarat.hrl").
-include("card.hrl").

-define(ANY(Total,Lists),lists:member(Total,Lists)).
-define(ADDCARD(Status,Pos,Card,Cards),{Status,Pos,maps:put(Pos,Card,Cards)}).

total(Cards)->
	casino_card:total(Cards,fun baccarat_card:value/1).

from_string(Cards)->
	[Pcs,Bcs]=string:tokens(Cards,"#"),
	CLp=lists:reverse(casino_card:string_to_cards(Pcs)),
	Mp=create_map(CLp,[?PLAYER_POS_1,?PLAYER_POS_2,?PLAYER_POS_3]),
	CLb=lists:reverse(casino_card:string_to_cards(Bcs)),
	Mb=create_map(CLb,[?BANKER_POS_1,?BANKER_POS_2,?BANKER_POS_3]),
	maps:merge(Mp,Mb).

create_map(Cards,IndexList)->
	case {Cards,IndexList} of
		{[],_} -> 
			#{};
		{[H1|T1],[H2|T2]} -> 
			Rest=create_map(T1,T2),
			maps:put(H2,H1,Rest)
	end.

cards_to_string(Pcs,Bcs)->
	PL=casino_card:cards_to_string(Pcs),
	BL=casino_card:cards_to_string(Bcs),
	lists:append([PL,"#",BL]).

to_string(#{?PLAYER_POS_1:=P1,?PLAYER_POS_2:=P2,?PLAYER_POS_3:=P3,?BANKER_POS_1:=B1,?BANKER_POS_2:=B2,?BANKER_POS_3:=B3})->
	cards_to_string([P3,P2,P1],[B3,B2,B1]);
to_string(#{?PLAYER_POS_1:=P1,?PLAYER_POS_2:=P2,?BANKER_POS_1:=B1,?BANKER_POS_2:=B2,?BANKER_POS_3:=B3})->
	cards_to_string([P2,P1],[B3,B2,B1]);
to_string(#{?PLAYER_POS_1:=P1,?PLAYER_POS_2:=P2,?PLAYER_POS_3:=P3,?BANKER_POS_1:=B1,?BANKER_POS_2:=B2})->
	cards_to_string([P3,P2,P1],[B2,B1]);
to_string(#{?PLAYER_POS_1:=P1,?PLAYER_POS_2:=P2,?BANKER_POS_1:=B1,?BANKER_POS_2:=B2})->
	cards_to_string([P2,P1],[B2,B1]).


put(Pos,Card,Cards)->
	casino_card:put(Pos,Card,Cards,?ALL_POS).

remove(Pos,Cards)->
	casino_card:remove(Pos,Cards).

check6cards(Pt,Bt,P3v)->
	Pt < 6 andalso 
	(Bt <3 orelse 
		(Bt==3 andalso P3v /=8) orelse 
		(Bt==4 andalso (not ?ANY(P3v,[0,1,8,9]))) orelse
		(Bt==5 andalso ?ANY(P3v,[4,5,6,7])) orelse 
		(Bt==6 andalso ?ANY(P3v,?TOTAL67))).

check4cards(Pt,Bt)->
	?ANY(Pt,?TOTAL89) orelse ?ANY(Bt,?TOTAL89) orelse (?ANY(Pt,?TOTAL67) andalso ?ANY(Bt,?TOTAL67)).

validate(Cards=#{?PLAYER_POS_1 :=P1,?PLAYER_POS_2 :=P2,?BANKER_POS_1 :=B1,?BANKER_POS_2 :=B2})->
	Bt=total([B1,B2]),
	Pt=total([P1,P2]),
	case {map_size(Cards),Cards} of
		{4,_}->
			check4cards(Pt,Bt);
		{5,#{?PLAYER_POS_3 := _}}->
			Pt < 6 andalso (not ?ANY(Bt,?TOTAL89));
		{5,#{?BANKER_POS_3 :=_}}->
			?ANY(Pt,?TOTAL67) andalso Bt < 6;
		{6,#{?PLAYER_POS_3 :=P3,?BANKER_POS_3 :=_}}->
			P3v=baccarat_card:value(P3),
			check6cards(Pt,Bt,P3v);
		_ -> false
	end;
validate(_Cards)->
	false.

add(Card,Cards) when is_map(Cards) andalso is_record(Card,card)->
	Calc4 = fun(Pt,Bt)->
		if
			(Pt== 6 orelse Pt == 7) andalso Bt < 6 -> 
				?ADDCARD(done,?BANKER_POS_3,Card,Cards);
			true ->
				case check6cards(Pt,Bt,baccarat_card:value(Card)) of
					true->
						?ADDCARD(more,?PLAYER_POS_3,Card,Cards);
					_ -> 
						?ADDCARD(done,?PLAYER_POS_3,Card,Cards)
				end
		end
	end,
	case {maps:size(Cards),Cards} of
		{0,#{}}-> 
			?ADDCARD(more,?PLAYER_POS_1,Card,Cards);
		{1,#{?PLAYER_POS_1 := _}}->
			?ADDCARD(more,?BANKER_POS_1,Card,Cards);
		{2,#{?PLAYER_POS_1 := _, ?BANKER_POS_1 := _}}->
			?ADDCARD(more,?PLAYER_POS_2,Card,Cards);
		{3,#{?PLAYER_POS_1 :=P1,?PLAYER_POS_2 :=P2,?BANKER_POS_1 := B1}}->
			case check4cards(total([P1,P2]),total([B1,Card])) of
				true ->
					?ADDCARD(done,?BANKER_POS_2,Card,Cards);
				false -> 
					?ADDCARD(more,?BANKER_POS_2,Card,Cards)
			end;
		{4,#{?PLAYER_POS_1 :=P1,?PLAYER_POS_2 :=P2,?BANKER_POS_1 :=B1,?BANKER_POS_2 := B2}}->
			Pt=total([P1,P2]),
			Bt=total([B1,B2]),
			case check4cards(Pt,Bt) of
				true->
					{error,Cards};
				false->
					Calc4(Pt,Bt)
			end;
		{5,#{?PLAYER_POS_1 :=P1,?PLAYER_POS_2 :=P2,?PLAYER_POS_3 :=P3,?BANKER_POS_1:=B1,?BANKER_POS_2:=B2}}->
			case check6cards(total([P1,P2]),total([B1,B2]),baccarat_card:value(P3)) of
				true->
					?ADDCARD(done,?BANKER_POS_3,Card,Cards);
				_ -> 
					{error,Cards}
			end	;
		_ -> 
			error
	end.