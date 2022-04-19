# filetype: shinyApp
## https://rstudio.github.io/shinytableau/articles/example.html

library(shiny)
library(shinytableau)
library(promises)
library(shinyvalidate)
library(ggplot2)

manifest <- tableau_manifest_from_yaml()


ui <- function(req) {
  fillPage(
    plotOutput("plot", height = "100%",
      brush = brushOpts("plot_brush", resetOnNew = TRUE)
    )
  )
}

server <- function(input, output, session) {
  df <- reactive_tableau_data("data_spec")

  observeEvent(input$plot_brush, {
    worksheet <- req(tableau_setting("data_spec")$worksheet)
    tableau_select_marks_by_brush_async(worksheet, input$plot_brush)
  })

  output$plot <- renderPlot({
    plot_title <- tableau_setting("plot_title")
    xvar <- tableau_setting("xvar")
    yvar <- tableau_setting("yvar")

    df() %...>% {
      ggplot(., aes(x = !!as.symbol(xvar), y = !!as.symbol(yvar))) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
        ggtitle(plot_title)
    }
  })
}

shinyApp(ui=ui, server=server)
