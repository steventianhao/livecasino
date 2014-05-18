create table bets (
	id bigint not null AUTO_INCREMENT,
	bet_bundle_id  bigint not null,
	player_table_id int not null,
	round_id varchar(32) not null, 
	player_id bigint not null,
	bet_amount decimal(32,2) not null,
	return_amount decimal(32,2),
	create_time bigint not null,
	PRIMARY KEY (id)
) ENGINE=InnoDB;