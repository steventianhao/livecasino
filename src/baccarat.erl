-module(baccarat).

-export([add/2]).
-export([ace/0,two/0,three/0,four/0,five/0,six/0,seven/0,eight/0,nine/0,ten/0,jack/0,queen/0,king/0]).

-define(INVALID_POS,-1).
-define(BANKER_POS_1,1).
-define(BANKER_POS_2,2).
-define(BANKER_POS_3,3).
-define(PLAYER_POS_1,4).
-define(PLAYER_POS_2,5).
-define(PLAYER_POS_3,6).

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

add(Card,Cards) when is_map(Cards) andalso is_record(Card,card)->
	Size= maps:size(Cards),
	case {Size,Cards} of
		{0,_}-> 
			{?PLAYER_POS_1,Cards#{?PLAYER_POS_1 => Card}};
		{1,#{?PLAYER_POS_1 := _}}->
			{?BANKER_POS_1,Cards#{?BANKER_POS_1 => Card}};
		{2,#{?PLAYER_POS_1 := _, ?BANKER_POS_1 := _}}->
			{?PLAYER_POS_2,Cards#{?PLAYER_POS_2 => Card}};
		{3,#{?PLAYER_POS_1 := _, ?BANKER_POS_1 := _, ?PLAYER_POS_2 :=_}}->
			{?BANKER_POS_2,Cards#{?BANKER_POS_2 => Card}};
		{4,#{?PLAYER_POS_1 :=P1, ?BANKER_POS_1 :=B1,?PLAYER_POS_2 :=P2,?BANKER_POS_2 := B2}}->
			Pos=calc_position([P1,P2],[B1,B2]),
			case Pos of
				?INVALID_POS -> {?INVALID_POS,Cards};
				?PLAYER_POS_3 -> {?PLAYER_POS_3,Cards#{?PLAYER_POS_3=>Card}};
				?BANKER_POS_3 -> {?BANKER_POS_3,Cards#{?BANKER_POS_3=>Card}}
			end;			
		{5,#{?PLAYER_POS_1:=P1,?PLAYER_POS_2:=P2,?PLAYER_POS_3:=P3,?BANKER_POS_1:=B1,?BANKER_POS_2:=B2}}->
			Pos=calc_position([P1,P2,P3],[B1,B2]),
			case Pos of
				?INVALID_POS -> {?INVALID_POS,Cards};
				?BANKER_POS_3 -> {?BANKER_POS_3,Cards#{?BANKER_POS_3=>Card}}
			end;
		_ -> {?INVALID_POS,Cards}
	end.

calc_position(Pcs=[_,_],Bcs=[_,_])->
	Pt=total(Pcs),
	Bt=total(Bcs),
	if
		Pt == 8 orelse Pt ==9 orelse Bt==8 orelse Bt==9 -> 
			?INVALID_POS;
		Pt== 6 orelse Pt == 7 ->
			if
				Bt < 6 -> ?BANKER_POS_3;
				true -> ?INVALID_POS
			end;
		true ->
			?PLAYER_POS_3
	end;
calc_position([_,_,P3],Bcs=[_,_])->
	case total(Bcs) of
		T  when T< 3 -> ?BANKER_POS_3;
		3 when P3#card.value /= 8 -> ?BANKER_POS_3;
		4 when P3#card.value /= 8 orelse P3#card.value /= 9 orelse P3#card.value /=1 -> ?BANKER_POS_3;
		5 when P3#card.value == 4 orelse P3#card.value == 5 orelse P3#card.value ==6 orelse P3#card.value ==7 -> ?BANKER_POS_3;
		6 when P3#card.value == 6 orelse P3#card.value == 7 -> ?BANKER_POS_3;
		_ ->?INVALID_POS
	end.