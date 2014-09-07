# This file will contain all the configuration parameters for the R code in DART-WA.

# DIRECTORIES

# RCD <- "C:\\Users\\asmita.ghodke\\Desktop\\indrajeet\\VSSHOME\\DARTWA\\R"
# This is "R code directory".
# This is where all the R code files will reside.
# This path is used internally by the R code for inter-file function calling.
# This path should be set when the DART-WA backend gets installed.

DbInsertTempDir <- "\\\\arlmssqldev02\\Data Transfer"
# This is temporary directory for database related files storage
# Use this directory before bulk insert into database.

RWD <- ""
# This is "R working directory".
# This is where all the data being read or written by R will reside.
# This path will be used by R to read files from and write files to.
# This path should be set when the DART-WA backend gets installed.

# DATABASE DETAILS
dbDriver <- 'driver={SQL Server};server=ARLMSSQLDEV02;database=DART_dev;uid=dartuser;pwd=dartuser#123'
# This is dbDriver credentials to connection to ODBC database.
# Here you can specify uid, password, server name and database name.
# This path is used for connection to database specified in connection string.
