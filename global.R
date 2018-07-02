
library(aws.s3)
library(sqldf)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(rhandsontable)


options(stringsAsFactors = FALSE)

# DATABASE CONNEXION
db <- dbConnect(SQLite(), dbname = "succotash.sqlite")
# https://www.r-bloggers.com/r-and-sqlite-part-1/

db_tables <- dbListTables(db)
if (!"tags" %in% db_tables) {
    query <- paste("CREATE TABLE tags",
                   paste("(ID INTEGER NOT NULL PRIMARY KEY",
                         "tag VARCHAR(255)",
                         sep = ", "),
                   ")")
    dbSendQuery(db, query)
}

# S3 CONNEXION
s3 <- get_bucket(bucket = "succotash-shiny")

onStop(function() {
    dbDisconnect(db)

})
