create table bet_limits (
	id bigint not null AUTO_INCREMENT,
	type char(1), -- T means table_limits, P means player_limits
	ref_id bigint not null,
	bet_type_id bigint not null,
	lmin decimal(32,2),
	lmax decimal(32,2),
	PRIMARY KEY (id)
) ENGINE=InnoDB;