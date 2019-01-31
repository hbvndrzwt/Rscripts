# install the required package
#install.packages("odbc")
#install.packages("data.table")
#install.packages("DBI")
#install.packages("rstudioapi")
#install.packages("plyr")

# Other required libraries
library(odbc)
library(data.table)
library(DBI)
library(rstudioapi)
library(dplyr, warn.conflicts = FALSE)
library(plyr)


Datalake_connect <- function(schema, table, password){
  

    # Connecting to the database
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "mxdatalake.database.windows.net",
                     Database = "mx-datalake",
                     # Fill your username in HERE
                     UID = "HuubVanDerZwet",
                     PWD = password,
                     Port = 1433)
    
    #Fill in the required schema and table here
    a <- DBI::Id( schema = schema, table = table)
    Dataset <- DBI::dbReadTable(con, a)

    return(Dataset)
}