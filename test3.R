library(shiny)
library(plotly)
library(dplyr)


n   <- 20
x   <- 1:n 
y   <- cumsum(rnorm(n))
z   <- runif(n,10,200)
cat <- sample(letters[1:5],n,replace = TRUE)
delete <- FALSE


df<-data.frame(cat,x,y,z, delete)

ui <- fluidPage(
  
  selectInput("var","var", c("y","z"), "y"),
  mainPanel(plotlyOutput("plot")),
  verbatimTextOutput("selection"),
  actionButton("delete","Delete", style = "display:inline-block;"),
  actionButton("reset","Reset", style = "display:inline-block;"),
  
  
)

server <- function(input, output, session) {
  
  myData <- reactiveValues(df = df)
  
  output$plot <- renderPlotly({
    
    plot_ly(myData$df, 
            x = ~x,
            y = ~get(input$var), 
            type = "scatter", 
            mode = "markers",
            text = ~cat,
            marker = list(size = 10),
            source = "A")
    
    
  })
  
  p1 <- reactive({
    
    event_data("plotly_click", source = "A")
    
  })
  
  p2 <- reactiveValues(points = c())
  
  observeEvent(p1(),{
    
    p2$points <- c(p2$points,as.list(p1())$pointNumber)
    
  })
  
  observeEvent(input$reset,{
    
    p2$points <- c()
    
  })
  
  output$selection <- renderPrint({ if(length(p2$points+1)<1){"Select data points to delete"}else{(p2$points+1)} })
  
  
  observeEvent(input$delete,{
    # browser()
    myData$df <- myData$df %>%
      mutate(delete = ifelse(row_number() %in% c(p2$points+1),TRUE,delete)) %>%
      filter(!delete)
    
    # And clear input?
    p2$points <- c()
  })
  
  
}

shinyApp(ui, server)