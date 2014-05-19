create table player_limits (
	id bigint not null AUTO_INCREMENT,
	player_id int not null,
	player_table_id not null,
	lmin decimal(32,2),
	lmax decimal(32,2),
	PRIMARY KEY (id)
) ENGINE=InnoDB;