library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click"),
  verbatimTextOutput("brush"),
  tableOutput('table'),
  actionButton('delete', 'DeleteData ? '), 
  actionButton('ResetData', 'Reset data')
)

server <- function(input, output, session) {
  
  nms <- row.names(mtcars)
  don=data.frame(x=mtcars$mpg,y=mtcars$wt)
  df=reactiveVal(don)
 
  output$plot <- renderPlotly({
    p <- ggplot(df(), aes(x = x, y = y)) + geom_point()
    ggplotly(p)
  })
  

  selectP <- reactive({
    d <- event_data("plotly_click")
    l <- event_data("plotly_selected")
    rbind(d,l)
    
  })
  
  result<- reactive({
    
    result=don %>% filter(!(mpg %in% selectP()$x & wt %in% selectP()$y)) %>% 
      mutate(x=mpg,y=wt)
    return(result)
  }) 
  

  observeEvent(input$delete,{
    # df()
    df(result())
  
  })
  
  observeEvent(input$ResetData, {
    df(don)
  })
  
  output$table<- renderTable({
    selectP()[,c('x','y')]
  })
  

  
  # 
  # output$brush <- renderPrint({
  #   d <- event_data("plotly_selected")
  #   if (!is.null(d)) d
  # })

  
}

shinyApp(ui, server)