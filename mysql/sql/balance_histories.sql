create table balance_histories (
	id bigint not null AUTO_INCREMENT,
	player_id  bigint not null,
	before decimal(32,2) not null,
	delta decimal(32,2) not null,
	after decimal(32,2) not null,
	create_time bigint not null,
	action smallint not null, -- payout, bet, ft_in, ft_out
	PRIMARY KEY (id)
) ENGINE=InnoDB;