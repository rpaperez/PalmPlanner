library(shiny)

runApp(list(
  ui=pageWithSidebar(headerPanel("Adding entries to table"),
                     sidebarPanel(textInput("text1", "Column 1"),
                                  textInput("text2", "Column 2"),
                                  actionButton("update", "Update Table")),
                     mainPanel(tableOutput("table1"))),
  server=function(input, output, session) {
    values <- reactiveValues()
    values$df <- data.frame(Column1 = NA, Column2 = NA)
    newEntry <- observe({
      if(input$update > 0) {
        newLine <- isolate(c(input$text1, input$text2))
        isolate(values$df <- rbind(values$df, newLine))
      }
    })
    output$table1 <- renderTable({values$df})
  }))