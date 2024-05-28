# Load packages -----------------------------------------------------------
# require("devtools")
# devtools::install_github("daattali/colourpicker")

packs <- c('shiny','shinythemes','shinycssloaders',"lubridate", "stringr", 'tidyverse','viridis','plotly',"colourpicker")
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack))}
lapply(packs, InstIfNec)
library(shiny)
shinyApp(
  ui = fluidPage(
    colourInput("col", "Select colour", "purple"),
    plotOutput("plot")
  ),
  server = function(input, output) {
    output$plot <- renderPlot({
      set.seed(1)
      plot(rnorm(50), bg = input$col, col = input$col, pch = 21)
    })
  }
)