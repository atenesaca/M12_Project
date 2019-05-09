# get shiny, DBI, dplyr and dbplyr from CRAN
install.packages("shiny")
install.packages("DBI")
install.packages("dplyr")
install.packages("dbplyr")
install.packages("devtools")
install.packages("RMySQL")

# get pool from GitHub, since it's not yet on CRAN
devtools::install_github("rstudio/pool")

