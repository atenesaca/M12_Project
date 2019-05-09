library(pool)
library(dplyr)

my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "laravel",
  host = "localhost",
  username = "root",
  password = "root"
)

# get the first 5 rows:
df_pelis = my_db %>% tbl("peliculas") %>% head
class(df_pelis)
df_pelis

#----------------

library(DBI)

conn <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "laravel",
  host = "localhost",
  username = "root",
  password = "root")

rs <- dbSendQuery(conn, "SELECT * FROM peliculas LIMIT 5;")

dbFetch(rs)
dbClearResult(rs)

dbGetQuery(conn, "SELECT * FROM peliculas LIMIT 5;")

