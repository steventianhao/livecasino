CREATE TABLE rounds (id bigint NOT NULL AUTO_INCREMENT, 
	shoe_index int NOT NULL, 
	round_index int NOT NULL, 
	dealer_table_id bigint NOT NULL, 
	dealer_id bigint NOT NULL, 
	status int DEFAULT '1' NOT NULL, 
	create_time bigint NOT NULL, 
	cards varchar(100), 
	finish_time bigint, 
	PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

