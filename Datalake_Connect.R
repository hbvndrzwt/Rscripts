
## Setup:
##   1.  in R, install package RJDBC and DBI
##   2.1 for datalake(Analytic's sql server), download mssql-jdbc-6.4.0.jre8.jar from internet and replace PATHTO with the path to the downloaded file. Fill in your user name and password in the user="" and pasword="".
##   2.2 for aws athena, download AthenaJDBC41_2.0.6.jar from internet and replace PATHTO with the corresponding path to the downloaded file. Fill in your secret key and secret key ID in the user="" and pasword="". replace STARTINGDIR with your own starting dir(the administrator will provide you the dir).
## Usage:
##  dbGetQuery(SQL_STATEMENT, SOURCE)
##
## Parameters:
##  SQL_STATEMENT, character, the sql statement you want to run.
##  SOURCE, character, the database you want to query from, can be either "datalate" or "athena", defalut "datalake"
## 
## Values:
##  return the query result as a data.frame
## 
## Examples:
##  dbGetQuery("show databases") #the default SOURCE is "datalake", so you don't need to specify SOURCE when querying from datalake
##  dbGetQuery("select top 3 * from community.Challenges_Experiment_Data")
##  dbGetQuery("show databases",SOURCE="athena")  # show databases from "athena"

install.packages("RJDBC")
install.packages("DBI")
install.packages("rJava")

library(DBI)
library(rJava)
library(RJDBC)

dbGetQuery <- function(...,SOURCE=c("datalake","athena")){
  options(java.parameters = "-Xmx8g")
  SOURCE <- match.arg(SOURCE)
  require(RJDBC)
  statement <- paste0(...)
  if(SOURCE=="datalake"){
    drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver","C://Users//hzw//Dropbox (Mendix)//Community//Onboarding//_Analytics//mssql-jdbc-6.4.0.jre8.jar")
    conn <- dbConnect(drv, url="jdbc:sqlserver://mxdatalake.database.windows.net", user="HuubVanDerZwet", password="RuGMXHZHPAaLUR5N",database="mx-datalake")
  }else if(SOURCE=="athena"){
    ## athena's syntax is similar to MySQL
    drv <- JDBC(driverClass="com.simba.athena.jdbc.Driver","PATHTO/AthenaJDBC41_2.0.6.jar", identifier.quote="'")
    conn <- dbConnect(drv, "jdbc:awsathena://AwsRegion=eu-west-1",
                      s3_staging_dir="STARTINGDIR",
                      user="",
                      password="")
  }
  res <- tryCatch(DBI::dbGetQuery(conn,statement),
                  warning=function(w){
                    print(w)
                    dbDisconnect(conn)
                    data.frame()
                  },
                  error=function(e){
                    print(e)
                    dbDisconnect(conn)
                    data.frame()
                  })
  try(dbDisconnect(conn),silent = TRUE)
  return(res)
}

dbGetQuery("select * from community.Challenges_Experiment_Data", "datalake")
