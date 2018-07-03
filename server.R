
function(input, output, session) {

    library(data.table)
    library(magrittr)
    library(DT)

    # ------ REACTIVE ---------------------------------------------------------

    values <- reactiveValues(
        tags = dbGetQuery(db, "SELECT * FROM tags") %>% data.table,
        recipes = dbGetQuery(db, "SELECT * FROM recipes") %>% data.table
    )

    # ------ UI ---------------------------------------------------------------

    # TABLE RECIPES
    output$ui_table_recipes <- renderUI({
        box(title = "Recipes",
            width = 12,
            dataTableOutput("table_recipes"))
    })

    # ADD NEW RECIPE
    output$ui_new_recipe <- renderUI({

        box(title = "New Recipe",
            textInput("new_title", "TITLE",
                      placeholder = "Title"),
            fluidRow(
                column(width = 6,
                       textInput("new_prep_time", "PREPARATION TIME",
                                 placeholder = "25 minutes")
                ),
                column(width = 6,
                       textInput("new_yield", "YIELD",
                                 placeholder = "4 cups")
                )
            ),
            div(style = "width: 40%; margin-right: 7px;", class = "inner",
                selectInput("new_tags", "TAGS", multiple = TRUE,
                            choices = sort(values$tags$tag))),
            div(class = "inner", style = "width: 5%;",
                actionLink("new_tags_add1",
                           label = img(src = "add.png",
                                       width = "15px",
                                       alt = "Add new tag",
                                       title = "Add new tag")),
                hidden(actionLink("new_tags_add2",
                                  label = img(src = "undo.png",
                                              width = "15px")))),
            div(class = "inner", style = "width: 35%; margin-right: 7px;",
                hidden(textInput("new_tags_input", label = "",
                                 placeholder = "Enter new tag"))),
            div(class = "inner", style = "width: 5%; padding-top: 5px;",
                hidden(actionButton("new_tags_submit", label = "Add"))),
            textAreaInput("new_ingredients", label = "INGREDIENTS",
                          placeholder = paste("Enter one ingredient per line:",
                                              "3 tomatoes",
                                              "50g butter", sep = "\n"),
                          resize = "vertical", rows = 6),
            textAreaInput("new_instructions", label = "INSTRUCTIONS",
                          placeholder = paste("Enter instructions steps.",
                                              "One step per line"),
                          resize = "vertical", rows = 6),
            fileInput("new_picture", label = "ADD A PICTURE", width = "70%",
                      accept = c("image/jpeg", "image/png")),
            actionButton("new_submit", label = "Save", width = "100%",
                         style = "background: lightgreen")
        )
    })

    # ------ TAGS -------------------------------------------------------------

    # EDIT TAGS
    output$ui_edit_tags <- renderUI({
        box(title = "Edit Tags",
            selectInput("list_tags", "TAGS", multiple = TRUE,
                        choices = values$tags$tag),
            actionButton("remove_tags_submit", "Delete Tags")
        )
    })

    # ADD TAGS LOGIC
    observeEvent(input$new_tags_add1, {
        shinyjs::hide("new_tags_add1")
        shinyjs::show("new_tags_add2")
        shinyjs::show("new_tags_input")
        shinyjs::show("new_tags_submit")
    })

    observeEvent(input$new_tags_add2, {
        shinyjs::show("new_tags_add1")
        shinyjs::hide("new_tags_add2")
        shinyjs::hide("new_tags_input")
        shinyjs::hide("new_tags_submit")
    })

    # SAVE NEW TAG
    observeEvent(input$new_tags_submit, {
        if (!input$new_tags_input %in% values$tags$tag) {
            values$tags <- rbind(values$tags,
                                 data.frame(ID = max(values$tags$ID, 0) + 1,
                                            tag = input$new_tags_input))
        }
        dbWriteTable(db, name = "tags", value = values$tags,
                     overwrite = TRUE)
    })

    # DELETE TAG
    observeEvent(input$remove_tags_submit, {
        values$tags <- values$tags[!tag %in% input$list_tags]
        dbWriteTable(db, name = "tags", value = values$tags,
                     overwrite = TRUE)
    })

    # ------ RECIPES ----------------------------------------------------------

    # DISPLAY RECIPES TABLE
    output$table_recipes <- renderDataTable({
        dt <- values$recipes[, .(title, prep_time, yield)]
        datatable(dt,
                  rownames = NULL,
                  colnames = c("Title", "Preparation time", "Yield"),
                  selection = "single",
                  options = list(
                      lengthMenu = list(c(10, 25, 50, 100, -1),
                                        c("10", "25", "50", "100", "All")),
                      pageLength = 10,
                      searching = FALSE))
    })

    # SAVE NEW RECIPE
    observeEvent(input$new_submit, {
        filename <- paste0(gsub("-| |:", "", substr(Sys.time(), 1, 20)),
                           "-", input$new_picture$name)
        put_object(input$new_picture$datapath,
                   filename,
                   bucket = "succotash-shiny",
                   acl = "public-read")
        url <- paste0("https://s3.eu-west-3.amazonaws.com/succotash-shiny/",
                      filename)
        df <- data.table(ID = max(values$recipes$ID, 0) + 1,
                         title = input$new_title,
                         prep_time = input$new_prep_time,
                         yield = input$new_yield,
                         ingredients = input$new_ingredients,
                         instructions = input$new_instructions,
                         picture = url)
        values$recipes <- rbind(values$recipes, df)
        dbWriteTable(db, name = "recipes", value = df,
                     append = TRUE)

    })

}
