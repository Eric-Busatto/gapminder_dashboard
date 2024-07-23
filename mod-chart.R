library(shiny)
library(ggplot2)


chartUI <- function(id) {
        # Unique variable name
        ns <- NS(id)
        fluidRow(
                plotOutput(outputId = ns("chart"))
        )
}


chartServer <- function(id, type, df, x_col_name, y_col_name, title) {
        moduleServer(
                id = id,
                module = function(input, output, session) {
                        # Chart logic
                        # Extract a common property
                        chart_theme <- ggplot2::theme(
                                plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                                axis.title.x = element_text(size = 15),
                                axis.title.y = element_text(size = 15),
                                axis.text.x = element_text(size = 12),
                                axis.text.y = element_text(size = 12)
                        )
                        
                        # Line chart
                        if (type == "line") {
                                output$chart <- renderPlot({
                                        ggplot(df(), aes_string(x = x_col_name, y = y_col_name)) +
                                                geom_line(color = "#f96000", size = 2) +
                                                geom_point(color = "#f96000", size = 5) +
                                                geom_label(
                                                        aes_string(label = y_col_name),
                                                        nudge_x = 0.25,
                                                        nudge_y = 0.25
                                                ) +
                                                labs(title = title) +
                                                theme_classic() +
                                                chart_theme
                                })
                                
                                # Bar chart
                        } else {
                                output$chart <- renderPlot({
                                        ggplot(df(), aes_string(x = x_col_name, y = y_col_name)) +
                                                geom_col(fill = "#0099f9") +
                                                geom_text(aes_string(label = y_col_name), vjust = 2, size = 6, color = "#ffffff") +
                                                labs(title = title) +
                                                theme_classic() +
                                                chart_theme
                                })
                        }
                }
        )
}

