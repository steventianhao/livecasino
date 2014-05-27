-ifndef (DB_HRL).
-define(DB_HRL,true).

-record(db_new_round_req,{shoe_index,round_index,dealer_id,dealer_table_id,create_time,status}).
-record(db_round_res,{id,shoe_index,round_index,dealer_id,dealer_table_id,create_time,status,finish_time,cards}).

-record(db_bet_req,{round_id,player_id,player_table_id,bet_cats,bet_amounts,total_amount}).
-record(db_bet_res,{bet_bundle_id,balance_before,balance_after}).

-endif.