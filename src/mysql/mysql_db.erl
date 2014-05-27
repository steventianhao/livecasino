-module(mysql_db).

-record(dbNewRound,{shoe_index,round_index,dealer_id,dealer_table_id,create_time}).
-record(dbRound,{id,shoe_index,round_index,dealer_id,dealer_table_id,create_time,status,finish_time,cards}).

-record(dbBetRequest,{round_id,player_id,player_table_id,bet_cats,bet_amounts,total_amount}).
-record(dbBetResponse,{bet_bundle_id,balance_before,balance_after}).

-export([add_pool/0]).
-export([insert_round/2,update_round/2,update_round/4,load_last_round/2]).
-export([user_bet/2]).

-include_lib("emysql/include/emysql.hrl").

-define(CONN,casino_db_pool).

add_pool()->
	Params=[{size,1},{user,"ts1"},{host,"192.168.1.12"},{password,"111111"},{database,"casino"},{encoding,utf8}],
	emysql:add_pool(?CONN,Params).

insert_round(Conn,DbNewRound)->
	Sql = <<"insert into rounds (shoe_index,round_index,dealer_id,dealer_table_id,create_time,status) values (?,?,?,?,?,1)">>,
	emysql:prepare(stmt_insert_round,Sql),
	#dbNewRound{shoe_index=ShoeIndex,round_index=RoundIndex,dealer_id=DealerId,dealer_table_id=DealerTableId,create_time=CreateTime}=DbNewRound,
	Result=emysql:execute(Conn,stmt_insert_round,[ShoeIndex,RoundIndex,DealerId,DealerTableId,CreateTime]),
	emysql:insert_id(Result).

update_round(Conn,Id)->
	Sql = <<"update rounds set status=2 where id = ? and status=1">>,
	emysql:prepare(stmt_update_round1,Sql),
	Result=emysql:execute(Conn,stmt_update_round1,[Id]),
	emysql:affected_rows(Result).


update_round(Conn,Id,Cards,FinishTime)->
	Sql = <<"update rounds set status=3,cards=?,finish_time=? where id= ? and status =2">>,
	emysql:prepare(stmt_update_round2,Sql),
	Result=emysql:execute(Conn,stmt_update_round2,[Cards,FinishTime,Id]),
	emysql:affected_rows(Result).

load_last_round(Conn,DealerTableId)->
	Sql = <<"select id,shoe_index,round_index,dealer_id,dealer_table_id,create_time,status,finish_time,cards from rounds where dealer_table_id =? order by id desc limit 1">>,
	emysql:prepare(stmt_load_last_round,Sql),
	Result=emysql:execute(Conn,stmt_load_last_round,[DealerTableId]),
	emysql:as_record(Result,dbRound,record_info(fields,dbRound)).

user_bet(Conn,#dbBetRequest{round_id=RoundId,player_id=PlayerId,player_table_id=PlayerTableId,bet_cats=BetCats,bet_amounts=BetAmounts,total_amount=TotalAmount})->
	Result=emysql:execute(Conn,"call bet(?,?,?,?,?,?)",[RoundId,PlayerId,PlayerTableId,BetCats,BetAmounts,TotalAmount]),
	case Result of
		#ok_packet{} ->
			{error,insufficient_balance};
		[R=#result_packet{},#ok_packet{}] ->
			{ok,emysql:as_record(R,dbBetResponse,record_info(fields,dbBetResponse))}
	end.

