create table bet_bundles (
	id bigint not null AUTO_INCREMENT,
	round_id  varchar(32) not null,
	player_id bigint not null,
	player_table_id int not null,
	bet_amount decimal(32,2) not null,
	return_amount decimal(32,2) not null,
	create_time bigint not null,
	PRIMARY KEY (id)
) ENGINE=InnoDB;