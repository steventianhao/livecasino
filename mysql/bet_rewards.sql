create table bet_rewards (
	bet_id  bigint not null,
	reward_id int not null,
	return_amount decimal(32,2),
	create_time bigint not null,
	PRIMARY KEY (bet_id,reward_id)
) ENGINE=InnoDB;