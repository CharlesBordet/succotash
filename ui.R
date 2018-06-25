
dashboardPage(

    title = "Succotash",
    skin = "purple",

    dashboardHeader(
        title = "Succotash"
    ),

    dashboardSidebar(
        sidebarMenu(
            menuItem("Recipes", tabName = "menu_recipes",
                     icon = icon("book")),
            menuItem("Add new recipe", tabName = "menu_add_recipe",
                     icon = icon("plus"))
        )
    ),

    dashboardBody(

        tabItems(
            tabItem(tabName = "menu_add_recipe",

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
                        )
                        # checkboxGroupInput("new_tags"),
                        # list d'ingrédients
                        # instructions
                        # photos
                    )
            )
        )

    )
)
