create table rounds (
	id varchar(32) not null,
	shoe_index int not null,
	round_index int not null,
	dealer_id bigint not null,
	dealer_table_id int not null,
	status int not null,
	cards varchar (100),
	create_time bigint not null,
	finish_time bigint,
	PRIMARY KEY (id)
) ENGINE=InnoDB;