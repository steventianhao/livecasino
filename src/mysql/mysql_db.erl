-module(mysql_db).

-record(dbNewRound,{shoeIndex,roundIndex,dealerId,dealerTableId,createTime}).
-export([insert_round/2,add_pool/0]).


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