library(dplyr)
library(shiny)
library(gapminder)
source("mod-chart.R")
source("mod-table.R")


ui <- fluidPage(
        sidebarLayout(
                sidebarPanel(
                        tags$h3("Shiny Module Showcase"),
                        tags$hr(),
                        selectInput(inputId = "continent", label = "Continent:", choices = unique(gapminder$continent), selected = "Europe")
                ),
                mainPanel(
                        tableUI(id = "table-data"),
                        chartUI(id = "chart-bar"),
                        chartUI(id = "chart-line")
                )
        )
)


server <- function(input, output, session) {
        # Filter the data set first
        data <- reactive({
                gapminder %>%
                        filter(continent %in% input$continent) %>%
                        group_by(year) %>%
                        summarise(
                                avg_life_exp = round(mean(lifeExp), digits = 0),
                                avg_gdp_percap = round(mean(gdpPercap), digits = 2)
                        )
        })
        
        # Data table
        tableServer(
                id = "table-data",
                df = data,
                colnames = c("Year", "Average life expectancy", "Average GDP per capita"),
                caption = "Gapminder datasets stats by year and continent"
        )
        
        # Bar chart
        chartServer(
                id = "chart-bar",
                type = "bar",
                df = data,
                x_col_name = "year",
                y_col_name = "avg_life_exp",
                title = "Average life expectancy over time"
        )
        
        # Line chart
        chartServer(
                id = "chart-line",
                type = "line",
                df = data,
                x_col_name = "year",
                y_col_name = "avg_gdp_percap",
                title = "Average GDP per capita over time"
        )
}


shinyApp(ui = ui, server = server)

