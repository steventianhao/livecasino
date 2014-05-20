-module(mysql_db).

-record(dbNewRound,{shoeIndex,roundIndex,dealerId,dealerTableId,createTime}).
-record(dbRound,{id,shoeIndex,roundIndex,dealerId,dealerTableId,createTime,status,finish_time,cards}).

-export([add_pool/0]).
-export([insert_round/2,update_round/2,update_round/4,load_last_round/2]).


add_pool()->
	emysql:add_pool(casino_pool, [{size,1},
				     {user,"ts1"},
				     {host,"192.168.1.12"},
				     {password,"111111"},
				     {database,"casino"},
				     {encoding,utf8}]),
	casino_pool.


insert_round(Conn,DbNewRound)->
	Sql = <<"insert into rounds (shoe_index,round_index,dealer_id,dealer_table_id,create_time) values (?,?,?,?,?)">>,
	emysql:prepare(stmt_insert_round,Sql),
	#dbNewRound{shoeIndex=ShoeIndex,roundIndex=RoundIndex,dealerId=DealerId,dealerTableId=DealerTableId,createTime=CreateTime}=DbNewRound,
	emysql:execute(Conn,stmt_insert_round,[ShoeIndex,RoundIndex,DealerId,DealerTableId,CreateTime]).

update_round(Conn,Id)->
	Sql = <<"update rounds set status=2 where id = ?">>,
	emysql:prepare(stmt_update_round1,Sql),
	emysql:execute(Conn,stmt_update_round1,[Id]).

update_round(Conn,Id,Cards,FinishTime)->
	Sql = <<"update rounds set status=3,cards=?,finish_time=? where id= ?">>,
	emysql:prepare(stmt_update_round2,Sql),
	emysql:execute(Conn,stmt_update_round2,[Cards,FinishTime,Id]).

load_last_round(Conn,DealerTableId)->
	Sql = <<"select id,shoe_index,round_index,dealer_id,dealer_table_id,create_time,status,finish_time,cards from rounds where dealer_table_id =? order by id desc limit 1">>,
	emysql:parepare(stmt_load_last_round,Sql),
	emysql:execute(Conn,stmt_load_last_round,[DealerTableId]).
		