-module(baccarat).

-export([add/2,put/3,remove/2,commit/1]).
-export([ace/0,two/0,three/0,four/0,five/0,six/0,seven/0,eight/0,nine/0,ten/0,jack/0,queen/0,king/0]).

-define(INVALID_POS,-1).
-define(BANKER_POS_1,1).
-define(BANKER_POS_2,2).
-define(BANKER_POS_3,3).
-define(PLAYER_POS_1,4).
-define(PLAYER_POS_2,5).
-define(PLAYER_POS_3,6).

-define(TOTAL89,[8,9]).
-define(TOTAL67,[6,7]).

-define(ALL_POS,[?BANKER_POS_1,?BANKER_POS_2,?BANKER_POS_3,?PLAYER_POS_1,?PLAYER_POS_2,?PLAYER_POS_3]).

-record(card,{name,value}).
-define(ACE,#card{name='A',value=1}).
-define(TWO,#card{name='2',value=2}).
-define(THREE,#card{name='3',value=3}).
-define(FOUR,#card{name='4',value=4}).
-define(FIVE,#card{name='5',value=5}).
-define(SIX,#card{name='6',value=6}).
-define(SEVEN,#card{name='7',value=7}).
-define(EIGHT,#card{name='8',value=8}).
-define(NINE,#card{name='9',value=9}).
-define(TEN,#card{name='T',value=0}).
-define(JACK,#card{name='J',value=0}).
-define(QUEEN,#card{name='Q',value=0}).
-define(KING,#card{name='K',value=0}).

ace()-> ?ACE.
two()-> ?TWO.
three()-> ?THREE.
four()-> ?FOUR.
five()-> ?FIVE.
six()-> ?SIX.
seven()->?SEVEN.
eight()->?EIGHT.
nine()->?NINE.
ten()->?TEN.
jack()->?JACK.
queen()->?QUEEN.
king()->?KING.

total(Cards) ->
	lists:foldl(fun(X,Sum)->X#card.value+Sum end,0,Cards) rem 10.

put(Pos,Card,Cards) when is_map(Cards) andalso is_record(Card,card) andalso is_integer(Pos)->
	case lists:member(Pos,?ALL_POS) of
		true->
			maps:put(Pos,Card,Cards);
		false->
			Cards
	end.

remove(Pos,Cards) when is_map(Cards) andalso is_integer(Pos)->
	maps:remove(Pos,Cards).

anyoneOf(Total,Lists) when is_integer(Total) andalso is_list(Lists)->
	lists:member(Total,Lists).

commit(Cards=#{?PLAYER_POS_1 :=P1,?PLAYER_POS_2 :=P2,?BANKER_POS_1 :=B1,?BANKER_POS_2 :=B2}) when is_map(Cards)->
	Size = maps:size(Cards),
	Bt=total([B1,B2]),
	Pt=total([P1,P2]),
	case {Size,Cards} of
		{4,_}->
			anyoneOf(Pt,?TOTAL89) orelse anyoneOf(Bt,?TOTAL89) orelse (anyoneOf(Pt,?TOTAL67) andalso anyoneOf(Bt,?TOTAL67));
		{5,#{?PLAYER_POS_3 :=P3}}->
			P3v=P3#card.value,
			Bt3=(Bt==3 andalso P3v ==8)  ,
			Bt4=(Bt==4 andalso anyoneOf(P3v,[0,1,8,9])),
			Bt5=(Bt==5 andalso anyoneOf(P3v,[0,1,2,3,8,9])),
			Bt6=(Bt==6 andalso anyoneOf(P3v,[0,1,2,3,4,5,8,9])),
			Pt < 6 andalso  (Bt3 orelse Bt4 orelse Bt5 orelse Bt6 orelse Bt==7);
		{5,#{?BANKER_POS_3 :=_}}->
			anyoneOf(Pt,?TOTAL67) andalso Bt < 6;
		{6,#{?PLAYER_POS_3 :=P3,?BANKER_POS_3 :=_}}->
			P3v=P3#card.value,
			Bt3=(Bt==3 andalso P3v /=8) ,
			Bt4=(Bt==4 andalso anyoneOf(P3v,[2,3,4,5,6,7])),
			Bt5=(Bt==5 andalso anyoneOf(P3v,[4,5,6,7])),
			Bt6=(Bt==6 andalso anyoneOf(P3v,?TOTAL67)),
			Pt < 6 andalso (Bt <3 orelse Bt3 orelse Bt4 orelse Bt5 orelse Bt6);
		_ -> false
	end.


add(Card,Cards) when is_map(Cards) andalso is_record(Card,card)->
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
			calc_position4(total([P1,P2]),total([B1,B2]));
		{5,#{?PLAYER_POS_1:=_,?PLAYER_POS_2:=_,?PLAYER_POS_3:=P3,?BANKER_POS_1:=B1,?BANKER_POS_2:=B2}}->
			calc_position5(total([B1,B2]),P3#card.value);
		_ -> 
			?INVALID_POS
	end,
	case Pos of
		?INVALID_POS -> {Pos,Cards};
		_ -> {Pos,maps:put(Pos,Card,Cards)}
	end.

calc_position4(Pt,Bt)->
	if
		Pt == 8 orelse Pt ==9 orelse Bt==8 orelse Bt==9 -> ?INVALID_POS;
		(Pt== 6 orelse Pt == 7) andalso Bt < 6 -> ?BANKER_POS_3;
		(Pt== 6 orelse Pt == 7) andalso Bt >= 6 -> ?INVALID_POS;
		true -> ?PLAYER_POS_3
	end.

calc_position5(Bt,P3v)->
	case Bt of
		T  when T< 3 -> ?BANKER_POS_3;
		3 when  P3v /= 8 -> ?BANKER_POS_3;
		4 when P3v /= 8 orelse P3v /= 9 orelse P3v /=1 -> ?BANKER_POS_3;
		5 when P3v == 4 orelse P3v == 5 orelse P3v ==6 orelse P3v ==7 -> ?BANKER_POS_3;
		6 when P3v == 6 orelse P3v == 7 -> ?BANKER_POS_3;
		_ ->?INVALID_POS
	end.