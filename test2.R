library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      width = 3, 
      menuItem(
        startExpanded = TRUE,
        selectInput("XVar", "Please Select X-Axis", 
                    choices = c("eins", "zwei", "drei"), selected = "eins"), 
        selectInput("YVar","Please Select Y-Axis",
                    choices = c("eins", "zwei", "drei"), selected = "zwei")
      )
    )
  ),
  dashboardBody(
    width=9,
    tabItem(
      tabName = 'box2',
      tabBox(
        width = 16,
        tabPanel(
          "Long time data", 
          plotOutput("myPlot", click = "plot1_click", 
                     brush = brushOpts(id = "plot1_brush")), 
          actionButton('DeleteSelectedData', 'DeleteData ? '), 
          actionButton('ResetData', 'Reset data'), 
          column(
            width = 6,
            h4("Brushed points"), 
            verbatimTextOutput("brush_info"))
        )
      )
    )        
  )
)
server <- function(input, output, session) {
  eins <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  zwei <- c(4, 6, 3, 4, 400, -500, 900, 8, 12, 5)
  drei <- c(989, 663, 74, 543, 222, 1541, 1515, 12, 525, 21)
  de <- data.frame(eins, zwei, drei)
  
  rx_de <- reactiveVal(de)
  
  output$myPlot <- renderPlot({
    ggplot(data = rx_de()) +
      geom_line(aes_string(x = input$XVar, y = input$YVar))
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(rx_de(), input$plot1_brush)[, c(input$XVar, input$YVar)]
  })
  
  observeEvent(input$DeleteSelectedData, {
    Var1 <- brushedPoints(rx_de(), input$plot1_brush, allRows = TRUE)
    rx_de(Var1[!Var1$selected_, names(Var1) != "selected_", drop = FALSE])
  })
  
  observeEvent(input$ResetData, {
    rx_de(de)
  })
}

shinyApp(ui, server)