-module(mysql_db).

-export([insert_round/2,update_round/3,update_round/4,load_last_round/2]).
-export([user_bet/2]).

-include("db.hrl").
-include_lib("emysql/include/emysql.hrl").

insert_round(Conn,#db_new_round_req{shoe_index=ShoeIndex,round_index=RoundIndex,dealer_id=DealerId,dealer_table_id=DealerTableId,create_time=CreateTime})->
	Sql = <<"insert into rounds (shoe_index,round_index,dealer_id,dealer_table_id,create_time,status) values (?,?,?,?,?,1)">>,
	emysql:prepare(stmt_insert_round,Sql),
	Result=emysql:execute(Conn,stmt_insert_round,[ShoeIndex,RoundIndex,DealerId,DealerTableId,CreateTime]),
	emysql:insert_id(Result).

update_round(Conn,Id,StopTime)->
	Sql = <<"update rounds set status=2,stop_time=? where id = ? and status=1">>,
	emysql:prepare(stmt_update_round1,Sql),
	Result=emysql:execute(Conn,stmt_update_round1,[StopTime,Id]),
	emysql:affected_rows(Result).


update_round(Conn,Id,Cards,FinishTime)->
	Sql = <<"update rounds set status=3,cards=?,finish_time=? where id= ? and status =2">>,
	emysql:prepare(stmt_update_round2,Sql),
	Result=emysql:execute(Conn,stmt_update_round2,[Cards,FinishTime,Id]),
	emysql:affected_rows(Result).

load_last_round(Conn,DealerTableId)->
	Sql = <<"select id,shoe_index,round_index,dealer_id,dealer_table_id,create_time,status,cards from rounds where dealer_table_id =? order by id desc limit 1">>,
	emysql:prepare(stmt_load_last_round,Sql),
	Result=emysql:execute(Conn,stmt_load_last_round,[DealerTableId]),
	emysql:as_record(Result,db_round_res,record_info(fields,db_round_res)).

user_bet(Conn,#db_bet_req{round_id=RoundId,player_id=PlayerId,player_table_id=PlayerTableId,bet_cats=BetCats,bet_amounts=BetAmounts,total_amount=TotalAmount})->
	Result=emysql:execute(Conn,"call bet(?,?,?,?,?,?)",[RoundId,PlayerId,PlayerTableId,BetCats,BetAmounts,TotalAmount]),
	case Result of
		#ok_packet{} ->
			{error,insufficient_balance};
		[R=#result_packet{},#ok_packet{}] ->
			Res=emysql:as_record(R,db_bet_res,record_info(fields,db_bet_res)),
			#db_bet_res{bet_bundle_id=BetBundleId,balance_after=BalanceAfter}=Res,
			{ok,{BetBundleId,BalanceAfter}}
	end.

