-module(dealer_json).
-include("dealer_json.hrl").
-export([ok_json/1,ok_json/2,err_json/1,err_json/2]).

ok_json(Kind)->
	jsx:encode([{?KIND,Kind},{?CODE,1}]).
ok_json(Kind,Others)->
	jsx:encode([{?KIND,Kind},{?CODE,1}|Others]).
err_json(Kind)->
	jsx:encode([{?KIND,Kind},{?CODE,-1}]).
err_json(Kind,Error)->
	jsx:encode([{?KIND,Kind},{?CODE,-1},{?ERROR,Error}]).