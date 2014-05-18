create table table_limits (
	id bigint not null AUTO_INCREMENT,
	player_table_id int not null,
	operator_id  bigint not null,
	currency_id int not null,
	lmin decimal(32,2),
	lmax decimal(32,2),
	PRIMARY KEY (id)
) ENGINE=InnoDB;