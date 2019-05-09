DROP DATABASE IF EXISTS cellfiles;

CREATE DATABASE cellfiles CHARACTER SET utf8 COLLATE utf8_general_ci;

use cellfiles;

-- Creacion tabla users
CREATE TABLE `users` (
`id` INT(2) NOT NULL AUTO_INCREMENT,
`name` VARCHAR(70) NOT NULL,
`password` VARCHAR(20) NOT NULL,
`rol` VARCHAR(20) NOT NULL,
`email` VARCHAR(40) NOT NULL,
PRIMARY KEY (`id`)
) ENGINE=InnoDB;

-- Insersion de datos
INSERT INTO `users`(`name`, `password`, `rol`, `email`) VALUES
("admin","Administrator0","admin","administrator@gmail.com"),
("andres","Tenesaca0","researcher","andres.t.b.15@gmail.com"),
("pablo","Rodriguez0","researcher","pablorodriguez@gmail.com");


-- Creacion tabla files
CREATE TABLE `files` (
`id` INT(2) NOT NULL AUTO_INCREMENT,
`filename` VARCHAR(50) NOT NULL,
`source` VARCHAR(20) NOT NULL,
`dbid` VARCHAR(20) NOT NULL,
PRIMARY KEY (`id`)
) ENGINE=InnoDB;

-- Insersion de datos

INSERT INTO `files`(`filename`, `source`, `dbid`) VALUES
("cancer","ArrayExpress","E-MTAB-7108"),
("prostate cancer","GeoQuery","GDS6100"),
("breats cancer","ArrayExpress","E-MTAB-6703");

-- Creacion tabla userfiles
CREATE TABLE `userfiles` (
`id` INT(2) NOT NULL AUTO_INCREMENT,
`userid` INT(2) NOT NULL,
`fileid` INT(2) NOT NULL,
PRIMARY KEY (`id`)
) ENGINE=InnoDB;

ALTER TABLE `userfiles`
ADD FOREIGN KEY (`userid`) REFERENCES users(`id`),
ADD FOREIGN KEY (`fileid`) REFERENCES files(`id`);


-- Insersion de userfiles

INSERT INTO `userfiles`(`userid`, `fileid`) VALUES
(2,1),
(3,3),
(3,2);
