CREATE TABLE IF NOT EXISTS books (
    id bigint(20) not null auto_increment,
    title varchar(512) not null,
    author varchar(256) not null,
    publisher varchar(256) not null,
    isbn varchar(17) not null,
    created_at datetime not null,
    updated_at datetime not null,
    PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
