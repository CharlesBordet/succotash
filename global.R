
library(aws.s3)
library(sqldf)
library(shiny)
library(shinyjs)
library(shinydashboard)


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
if (!"recipes" %in% db_tables) {
    query <- paste("CREATE TABLE recipes",
                   paste("(ID INTEGER NOT NULL PRIMARY KEY",
                         "title VARCHAR(255)",
                         "prep_time VARCHAR(255)",
                         "yield VARCHAR(255)",
                         "ingredients TEXT",
                         "instructions TEXT",
                         "picture VARCHAR(255)",
                         sep = ", "),
                   ")")
    dbSendQuery(db, query)
}

onStop(function() {
    dbDisconnect(db)
})

# S3 CONNEXION
s3 <- get_bucket(bucket = "succotash-shiny")

