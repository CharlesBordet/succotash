
function(input, output, session) {

    library(data.table)
    library(magrittr)
    library(DT)

    # ------ REACTIVE ---------------------------------------------------------

    values <- reactiveValues(
        all_tags = dbGetQuery(db, "SELECT * FROM tags") %>% data.table,
        all_recipes = dbGetQuery(db, "SELECT * FROM recipes") %>% data.table
    )

    # FILTERED RECIPES
    filtered_recipes <- reactive({
        input$filters_submit
        input$del_recipe
        filters_tags <- isolate(input$filters_tags)
        if (is.null(filters_tags)) {
            filters_tags <- values$all_tags$ID
        }
        query <- paste("SELECT * FROM recipes",
                       "WHERE recipes.ID IN (",
                       paste("SELECT recipe_id FROM tags_recipes",
                             "WHERE tag_id IN (",
                             paste(filters_tags, collapse = ","),
                             ")"),
                       ")")
        dbGetQuery(db, query) %>% data.table
    })

    # ------ UI ---------------------------------------------------------------

    # FILTERS FOR RECIPES
    output$filters_recipes <- renderUI({
        tagList(
            selectInput("filters_tags", "TAGS",
                        choices = setNames(values$all_tags$ID, values$all_tags$tag),
                                           multiple = TRUE),
            actionButton("filters_submit", "Filter")
        )
    })

    # ADD NEW RECIPE
    output$ui_new_recipe <- renderUI({

        input$new_submit
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
                            choices = sort(values$all_tags$tag))),
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
                        choices = values$all_tags$tag),
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
        if (!input$new_tags_input %in% values$all_tags$tag) {
            tag_id <- max(values$all_tags$ID, 0) + 1
            values$max_tags_id <- values$max_tags_id + 1
            df <- data.frame(ID = tag_id,
                             tag = input$new_tags_input)
            values$all_tags <- rbind(values$all_tags, df)
            dbWriteTable(db, name = "tags", value = df, append = TRUE)
        }
    })

    # DELETE TAG
    observeEvent(input$remove_tags_submit, {
        tags_deleted <- values$all_tags[tag %in% input$list_tags]$ID
        values$all_tags <- values$all_tags[!tag %in% input$list_tags]
        rs <- dbSendQuery(db, paste("DELETE FROM tags WHERE ID IN (",
                                    paste(tags_deleted, collapse = ","),
                                    ")"))
        dbClearResult(rs)
    })

    # ------ RECIPES ----------------------------------------------------------

    # DISPLAY RECIPES TABLE
    output$table_recipes <- DT::renderDataTable({
        dt <- req(filtered_recipes())[, .(title, prep_time, yield, delete = ID)]
        inputs <- character(nrow(dt))
        for (i in seq_len(nrow(dt))) {
            inputs[i] <- actionButton(
                paste0("del_recipe_", i),
                label = img(src = "cross_small.png"),
                onclick = "Shiny.onInputChange(\"del_recipe\", this.id)") %>%
                as.character
        }
        dt[, delete := inputs]

        datatable(dt,
                  rownames = NULL,
                  colnames = c("Title", "Preparation time", "Yield", "Delete"),
                  selection = "single",
                  escape = FALSE,
                  options = list(
                      lengthMenu = list(c(10, 25, 50, 100, -1),
                                        c("10", "25", "50", "100", "All")),
                      pageLength = 10,
                      searching = FALSE))
    })

    # DISPLAY RECIPE DETAILS
    output$recipe <- renderUI({
        recipes <- req(filtered_recipes())[req(input$table_recipes_rows_selected)]
        tagList(
            h1(recipes$title),
            p(strong("Preparation Time:"), recipes$prep_time),
            p(strong("Yield:"), recipes$yield),
            img(src = recipes$picture, width = 600),
            h3("INGREDIENTS"),
            tags$ul(lapply(strsplit(recipes$ingredients, "\\n")[[1]], tags$li)),
            h3("INSTRUCTIONS"),
            tags$ol(lapply(strsplit(recipes$instructions, "\\n")[[1]], tags$li))
        )
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
        recipe_id <- max(values$all_recipes$ID, 0) + 1
        df <- data.table(ID = recipe_id,
                         title = input$new_title,
                         prep_time = input$new_prep_time,
                         yield = input$new_yield,
                         ingredients = input$new_ingredients,
                         instructions = input$new_instructions,
                         picture = url)
        values$all_recipes <- rbind(values$all_recipes, df)
        dbWriteTable(db, name = "recipes", value = df,
                     append = TRUE)
        df2 <- data.table(tag_id = values$all_tags[tag %in% input$new_tags]$ID,
                          recipe_id = recipe_id)
        dbWriteTable(db, name = "tags_recipes", value = df2,
                     append = TRUE)
        updateTabItems(session, "tabs", "menu_recipes")
    })

    # DELETE RECIPE
    observeEvent(input$del_recipe, {
        selected_row <- as.numeric(strsplit(input$del_recipe, "_")[[1]][3])
        # Remove picture on S3
        s3_filename <- basename(filtered_recipes()[selected_row]$picture)
        delete_object(s3_filename,
                      bucket = "succotash-shiny")
        # Remove entry in recipes
        selected_id <- filtered_recipes()[selected_row]$ID
        rs <- dbSendQuery(db, paste("DELETE FROM recipes WHERE ID =", selected_id))
        dbClearResult(rs)
        values$recipes <- values$recipes[- selected_row]
        # Remove entry in tags_recipes
        rs <- dbSendQuery(db, paste("DELETE FROM tags_recipes WHERE recipe_id =",
                                    selected_id))
        dbClearResult(rs)
    })

}
