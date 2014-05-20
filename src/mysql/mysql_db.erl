-module(mysql_db).

-record(dbNewRound,{shoe_index,round_index,dealer_id,dealer_table_id,create_time}).
-record(dbRound,{id,shoe_index,round_index,dealer_id,dealer_table_id,create_time,status,finish_time,cards}).

-export([add_pool/0]).
-export([insert_round/2,update_round/2,update_round/4,load_last_round/2]).
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
		