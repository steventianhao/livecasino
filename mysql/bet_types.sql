create table bet_types (
	id bigint not null,
	name varchar(64) not null,
	game_id int not null,
	PRIMARY KEY (id)
) ENGINE=InnoDB;