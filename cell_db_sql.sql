-- DROP DATABASE IF EXISTS cellfiles;

-- CREATE DATABASE cellfiles CHARACTER SET utf8 COLLATE utf8 _general_ci;

-- use cellfiles;

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
('Identification of genome wide Hoxa9 binding sites in primary murine and human AML cells', 'ArrayExpress', 'E-MTAB-7108'),
('MicroRNA-135b overexpression effect on prostate ca', 'Geo Datasets', 'GDS6100'),
('A microarray meta-dataset of breast cancer', 'ArrayExpress', 'E-MTAB-6703'),
('Diet-induced obesity model: white adipose tissue', 'Geo Datasets', 'GDS6247'),
('Pulmonary dendritic cell subsets', 'Geo Datasets', 'GDS5663'),
('Human and chimpanzee TS12KOS vector-generated induced pluripotent stem cells', 'Geo Datasets', 'GDS5443'),
('Renal clear cell carcinoma (HG-U133B)', 'Geo Datasets', 'GDS507');

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
