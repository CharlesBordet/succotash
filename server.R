
function(input, output, session) {

    library(data.table)
    library(magrittr)
    library(DT)

    # ------ REACTIVE ---------------------------------------------------------

    values <- reactiveValues(
        all_tags = dbGetQuery(db, "SELECT * FROM tags") %>% data.table,
        all_recipes = dbGetQuery(db, "SELECT * FROM recipes") %>% data.table,
        all_ingredients = dbGetQuery(db, "SELECT * FROM ingredients") %>%
            data.table,
        ingredients_nb = 5
    )

    # INGREDIENTS NUMBER IN THE RECIPE CREATOR
    observeEvent(input$new_ingredient_add, {
        values$ingredients_nb <- values$ingredients_nb + 1
    })
    observeEvent(input$new_submit, {
        values$ingredients_nb <- 5
    })

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

    # INGREDIENTS UI
    ingredients_ui <- reactive({
        ingredients <- values$all_ingredients
        setorder(ingredients, name)
        lapply(1:values$ingredients_nb, function(i) {
            if (i == 1) {
                label1 <- "INGREDIENTS"; label2 <- "QUANTITY"
            } else {
                label1 <- NULL; label2 <- NULL
            }
            fluidRow(
                column(width = 6,
                       selectInput(paste0("new_ingredient_", i),
                                   label = label1,
                                   choices = setNames(ingredients$ID,
                                                      ingredients$name))),
                column(width = 6,
                       textInput(paste0("new_ingredient_qt_", i),
                                 label = label2, placeholder = "3 or 60g"))
            )
        })
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
            ingredients_ui(),
            actionButton("new_ingredient_add", label = "Add an ingredient"),
            br(), br(),
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

    # ADD INGREDIENT
    output$ui_add_ingredient <- renderUI({
        tagList(
            fluidRow(
                column(width = 4,
                       textInput("add_ingredient_name", "NAME",
                                 placeholder = "Tomato", width = "100%")),
                column(width = 4,
                       textInput("add_ingredient_weight", "BASE WEIGHT (g)",
                                 placeholder = "60", width = "100%")),
                column(width = 4,
                       textInput("add_ingredient_calories", "CALORIES (kcal per 100g)",
                                 placeholder = "18", width = "100%"))
            ),
            fluidRow(
                column(width = 4,
                       textInput("add_ingredient_carbs", "CARBOHYDRATES (g per 100g)",
                                 placeholder = "3.9", width = "100%")),
                column(width = 4,
                       textInput("add_ingredient_protein", "PROTEIN (g per 100g)",
                                 placeholder = "0.9", width = "100%")),
                column(width = 4,
                       textInput("add_ingredient_fat", "FAT (g per 100g)",
                                 placeholder = "0.2", width = "100%"))
            ),
            actionButton("add_ingredient_submit", label = "Save", width = "100%",
                         style = "background: lightgreen"),
            br(), br()
        )
    })

    # DISPLAY INGREDIENTS
    output$ui_list_ingredients <- renderUI({
        box(title = "Ingredients",
            width = NULL,
            DT::dataTableOutput("list_ingredients")
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
        browser()
        # Save picture on S3
        if (!is.null(input$new_picture)) {
            filename <- paste0(gsub("-| |:", "", substr(Sys.time(), 1, 20)),
                               "-", input$new_picture$name)
            put_object(input$new_picture$datapath,
                       filename,
                       bucket = "succotash-shiny",
                       acl = "public-read")
            url <- paste0("https://s3.eu-west-3.amazonaws.com/succotash-shiny/",
                          filename)
        } else {
            url <- NA
        }
        # Get ingredients
        recipe_id <- max(values$all_recipes$ID, 0) + 1
        ingredient_ids <- c()
        ingredient_qt <- c()
        i <- 1
        while (!is.null(input[[paste0("new_ingredient_", i)]])) {
            ingredient_ids <- c(ingredient_ids, input[[paste0("new_ingredient_", i)]])
            ingredient_qt <- c(ingredient_qt, input[[paste0("new_ingredient_qt_", i)]])
            i <- i + 1
        }
        # Save data in recipes table
        df <- data.table(ID = recipe_id,
                         title = input$new_title,
                         prep_time = input$new_prep_time,
                         yield = input$new_yield,
                         instructions = input$new_instructions,
                         picture = url)
        values$all_recipes <- rbind(values$all_recipes, df)
        dbWriteTable(db, name = "recipes", value = df, append = TRUE)
        # Save data in tags table
        df2 <- data.table(tag_id = values$all_tags[tag %in% input$new_tags]$ID,
                          recipe_id = recipe_id)
        dbWriteTable(db, name = "tags_recipes", value = df2, append = TRUE)
        # Save data in ingredients table
        df3 <- data.table(ingredient_id = ingredient_ids,
                          recipe_id = recipe_id,
                          quantity = ingredient_qt)
        dbWriteTable(db, name = "ingredients_recipes", value = df3, append = TRUE)
        # Redirect to recipes list
        updateTabItems(session, "tabs", "menu_recipes")
    })

    # DELETE RECIPE
    observeEvent(input$del_recipe, {
        browser()
        selected_row <- as.numeric(strsplit(input$del_recipe, "_")[[1]][3])
        # Remove picture on S3
        s3_filename <- basename(filtered_recipes()[selected_row]$picture)
        if (!is.na(s3_filename)) {
            delete_object(s3_filename, bucket = "succotash-shiny")
        }
        # Remove entry in recipes
        selected_id <- filtered_recipes()[selected_row]$ID
        rs <- dbSendQuery(db, paste("DELETE FROM recipes WHERE ID =", selected_id))
        dbClearResult(rs)
        values$all_recipes <- values$all_recipes[- selected_row]
        # Remove entry in tags_recipes
        rs <- dbSendQuery(db, paste("DELETE FROM tags_recipes WHERE recipe_id =",
                                    selected_id))
        dbClearResult(rs)
        # Remove entry in ingredients_recipes
        rs <- dbSendQuery(db, paste("DELETE FROM ingredients_recipes WHERE recipe_id =",
                                    selected_id))
        dbClearResult(rs)
    })

    # ------ INGREDIENTS ------------------------------------------------------

    # LIST OF INGREDIENTS
    output$list_ingredients <- DT::renderDataTable({
        # browser()
        dt <- values$all_ingredients[, .(name, weight, calories, carbs,
                                         protein, fat, delete = ID)]
        inputs <- character(nrow(dt))
        for (i in seq_len(nrow(dt))) {
            inputs[i] <- actionButton(
                paste0("del_ingredient_", i),
                label = img(src = "cross_small.png"),
                onclick = "Shiny.onInputChange(\"del_ingredient\", this.id)") %>%
                as.character
        }
        dt[, delete := inputs]

        datatable(dt,
                  rownames = NULL,
                  colnames = c("Name", "Base weight", "Calories", "Carbs",
                               "Protein", "Fat", "Delete"),
                  selection = "single",
                  escape = FALSE,
                  options = list(
                      lengthMenu = list(c(10, 25, 50, 100, -1),
                                        c("10", "25", "50", "100", "All")),
                      pageLength = 10,
                      searching = FALSE))
    })

    # ADD NEW INGREDIENT
    observeEvent(input$add_ingredient_submit, {
        df <- data.table(
            ID = max(values$all_ingredients$ID, 0) + 1,
            name = input$add_ingredient_name,
            weight = as.numeric(input$add_ingredient_weight),
            calories = as.numeric(input$add_ingredient_calories),
            carbs = as.numeric(input$add_ingredient_carbs),
            protein = as.numeric(input$add_ingredient_protein),
            fat = as.numeric(input$add_ingredient_fat)
        )
        values$all_ingredients <- rbind(values$all_ingredients, df)
        dbWriteTable(db, name = "ingredients", value = df,
                     append = TRUE)
    })

    # DELETE INGREDIENT
    observeEvent(input$del_ingredient, {
        selected_row <- as.numeric(strsplit(input$del_ingredient, "_")[[1]][3])
        selected_id <- values$all_ingredients[selected_row]$ID
        rs <- dbSendQuery(db, paste("DELETE FROM ingredients WHERE ID =", selected_id))
        dbClearResult(rs)
        values$all_ingredients <- values$all_ingredients[- selected_row]
    })
}
