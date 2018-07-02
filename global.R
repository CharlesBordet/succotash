
library(sqldf)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(rhandsontable)


options(stringsAsFactors = FALSE)

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

onStop(function() {
    dbDisconnect(db)

})
