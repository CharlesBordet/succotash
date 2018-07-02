
library(sqldf)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(rhandsontable)


options(stringsAsFactors = FALSE)

db <- dbConnect(SQLite(), dbname = "succotash.sqlite")
# https://www.r-bloggers.com/r-and-sqlite-part-1/