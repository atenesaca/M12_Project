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
  dbname = "cellfiles",
  host = "localhost",
  username = "root",
  password = "root")

rs <- dbSendQuery(conn, "SELECT * FROM users")

dbFetch(rs)
dbClearResult(rs)

username = "andres"
password = "Tenesaca0"
query = sprintf("SELECT * FROM users where name = ('%s') and password = ('%s')",
                username, password)
user = dbGetQuery(conn, query)
class(user)
user$name
user$password


dbGetQuery(conn,
"INSERT INTO users (name, password ,rol ,email)
  VALUES ('prueba', 'prueba1', 'researcher', 'prueba@gmail.com')"
)
