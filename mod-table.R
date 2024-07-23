library(shiny)
library(DT)


tableUI <- function(id) {
        # Unique variable name
        ns <- NS(id)
        fluidRow(
                DTOutput(outputId = ns("table"))
        )
}


tableServer <- function(id, df, colnames, caption) {
        moduleServer(
                id = id,
                module = function(input, output, session) {
                        # Render a table
                        output$table <- renderDT({
                                datatable(
                                        data = df(),
                                        colnames = colnames,
                                        caption = caption,
                                        filter = "top"
                                )
                        })
                }
        )
}

