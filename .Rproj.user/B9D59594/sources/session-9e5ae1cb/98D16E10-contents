library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush")
)

server <- function(input, output, session) {
  
  nms <- row.names(mtcars)
  
  output$plot <- renderPlotly({
    p <- ggplot(mtcars, aes(x = mpg, y = wt, key = nms)) + geom_point()
    ggplotly(p) %>% layout(dragmode = "lasso")
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (!is.null(d)) d
  })
  
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (!is.null(d)) d
  })
  
}

shinyApp(ui, server)