
library(aws.s3)
library(sqldf)
library(shiny)
library(shinyjs)
library(shinydashboard)

options(stringsAsFactors = FALSE)

# DATABASE CONNEXION
db <- dbConnect(SQLite(), dbname = "succotash.sqlite")

db_tables <- dbListTables(db)
if (!"tags" %in% db_tables) {
    query <- paste("CREATE TABLE tags",
                   paste("(ID INTEGER NOT NULL PRIMARY KEY",
                         "tag VARCHAR(255)",
                         sep = ", "),
                   ")")
    rs <- dbSendQuery(db, query)
    dbClearResult(rs)
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
    rs <- dbSendQuery(db, query)
    dbClearResult(rs)
}
if (!"tags_recipes" %in% db_tables) {
    query <- paste("CREATE TABLE tags_recipes",
                   paste("(tag_id INTEGER NOT NULL",
                         "recipe_id INTEGER NOT NULL",
                         "PRIMARY KEY (tag_id, recipe_id)",
                         "FOREIGN KEY (tag_id) REFERENCES tags(ID)",
                         "FOREIGN KEY (recipe_id) REFERENCES recipes(ID)",
                         sep = ", "),
                   ")")
    rs <- dbSendQuery(db, query)
    dbClearResult(rs)
}
if (!"ingredients" %in% db_tables) {
    query <- paste("CREATE TABLE ingredients",
                   paste("(ID INTEGER NOT NULL PRIMARY KEY",
                         "name VARCHAR(255) NOT NULL",
                         "weight REAL NOT NULL",
                         "calories REAL",
                         "carbs REAL",
                         "protein REAL",
                         "fat REAL",
                         sep = ", "),
                   ")")
    rs <- dbSendQuery(db, query)
    dbClearResult(rs)
}


onStop(function() {
    dbDisconnect(db)
})

# S3 CONNEXION
s3 <- get_bucket(bucket = "succotash-shiny")

