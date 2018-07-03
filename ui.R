
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
                     icon = icon("plus")),
            menuItem("Administration", tabName = "admin",
                     icon = icon("user"),
                     menuSubItem("Tags", tabName = "admin_tags"),
                     menuSubItem("Ingredients", tabName = "admin_ingredients")
            )
        )
    ),

    dashboardBody(

        useShinyjs(),

        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),

        tabItems(
            tabItem(tabName = "menu_recipes",
                    fluidRow(
                        column(width = 12,
                               box(title = "Recipes",
                                   width = NULL,
                                   DT::dataTableOutput("table_recipes")),
                               box(width = NULL,
                                   uiOutput("recipe"))
                        )
                    )
            ),

            tabItem(tabName = "menu_add_recipe",
                    fluidRow(uiOutput("ui_new_recipe"))
            ),

            tabItem(tabName = "admin_tags",
                    fluidRow(uiOutput("ui_edit_tags"))
            )
        )

    )
)
