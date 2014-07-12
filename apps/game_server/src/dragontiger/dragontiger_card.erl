-module(dragontiger_card).
-export([value/1]).
-include("card.hrl").

value(#card{rank=?ACE})->1;
value(#card{rank=?TWO})->2;
value(#card{rank=?THREE})->3;
value(#card{rank=?FOUR})->4;
value(#card{rank=?FIVE})->5;
value(#card{rank=?SIX})->6;
value(#card{rank=?SEVEN})->7;
value(#card{rank=?EIGHT})->8;
value(#card{rank=?NINE})->9;
value(#card{rank=?TEN})->10;
value(#card{rank=?JACK})->11;
value(#card{rank=?QUEEN})->12;
value(#card{rank=?KING})->13.