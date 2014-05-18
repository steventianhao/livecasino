create table player_tables (
	id int not null,
	payout_name varchar(64) not null,
	dealer_table_id int not null,
	primary key (id)
) ENGINE=InnoDB;