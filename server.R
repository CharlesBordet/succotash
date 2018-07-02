
function(input, output, session) {

    library(data.table)
    library(magrittr)

    # ------ Reactive ---------------------------------------------------------

    values <- reactiveValues(
        tags = readLines("./data/tags.txt")
    )

    # ------ UI ---------------------------------------------------------------

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
                       textInput("new_yield", "SERVINGS",
                                 placeholder = "4 cups")
                )
            ),
            div(style = "width: 40%; margin-right: 7px;", class = "inner",
                selectInput("new_tags", "TAGS", multiple = TRUE,
                            choices = values$tags)),
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

    # EDIT TAGS
    output$ui_edit_tags <- renderUI({
        box(title = "Edit Tags",
            selectInput("list_tags", "TAGS", multiple = TRUE,
                        choices = values$tags),
            actionButton("remove_tags_submit", "Delete Tags")
        )
    })

    # ADD TAGS LOGIC
    observeEvent(input$new_tags_add1, {
        hide("new_tags_add1")
        show("new_tags_add2")
        show("new_tags_input")
        show("new_tags_submit")
    })

    observeEvent(input$new_tags_add2, {
        show("new_tags_add1")
        hide("new_tags_add2")
        hide("new_tags_input")
        hide("new_tags_submit")
    })

    # SAVE NEW TAG
    observeEvent(input$new_tags_submit, {
        values$tags <- sort(unique(c(values$tags, input$new_tags_input)))
        writeLines(values$tags, "./data/tags.txt")
    })

    # DELETE TAG
    observeEvent(input$remove_tags_submit, {
        selected_tags <- input$list_tags
        values$tags <- setdiff(values$tags, selected_tags)
        writeLines(values$tags, "./data/tags.txt")
    })

    # SAVE NEW RECIPE
    observeEvent(input$new_submit, {

    })

}
