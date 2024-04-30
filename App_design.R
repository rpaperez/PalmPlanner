# Load packages -----------------------------------------------------------

packs <- c('shiny','shinythemes','shinycssloaders',"lubridate", "stringr", 'tidyverse','viridis','plotly')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack))}
lapply(packs, InstIfNec)


ui<-shinyUI(
  # fluidPage(
  
  # titlePanel("Simulate light transmission"),
  navbarPage(theme = shinytheme("sandstone"),
             title ="Designing oil palm based intercropping systems",
             sidebarLayout(
               
               sidebarPanel('Oil palm ',
                 numericInput("inter_dist",label = "Inter row distance (m):", value=10,step = 1),
                 numericInput("intra_dist",label = "Intra row distance (m):", value=5,step = 1),
                 numericInput("dist_intercrop",label = "Intercropping distance (m):", value=0,step = 1),
                 numericInput("lim",label = "plot limits (m):", value=50,step = 10),
                 selectInput(inputId="orientation", label = h4("Select scene orientation"),
                             choices = list("North-South" = 'NS',
                                            "East-West" = 'EW'),
                             selected = 'North-South'),
                 selectInput(inputId="designType", label = h4("Select an design pattern"),
                             choices = list("square" = 'square',
                                            'quincunx'='quincunx',
                                            "square2" = 'square2',
                                            'quincunx2'='quincunx2',
                                            'quincunx3'='quincunx3',
                                            'quincunx4'='quincunx4',
                                            'quincunx5'='quincunx5'),
                             selected = 'quincunx'),
                 
                 uiOutput('ui.action'),
               ),
               mainPanel(
                        plotlyOutput('plot')%>% withSpinner(color="darkorange")
                 
               )
             )
  )
)


server<-function(input, output,session){
  
  source('./1-code/helpers_App_Design.R')
  
  inter_dist<- reactive({
    input$inter_dist
    print(input$inter_dist)
  })
  
  intra_dist<- reactive({
    input$intra_dist
  })
  
  dist_intercrop<- reactive({
    input$dist_intercrop
  })
  
  orientation<- reactive({
    input$orientation
  })
  
  designType<- reactive({
    input$designType
  })
  
  lim<- reactive({
    input$lim
  })

  # generate the data 
  result<- reactive({
    input$action
    isolate({
      d_inter=inter_dist()
      d_intra=intra_dist()
      dist_intercrop=dist_intercrop()
      orientation=orientation()
      designType=designType()
      lim=lim()
      
      if (is.null(d_inter) | is.null(d_intra)) return(NULL)
      
      ## lunch simu
      
      #### map
      print(paste('dist_intra',d_intra))
      print(paste('dist_inter',d_inter))
      print(paste('dist_intercrop',dist_intercrop))
      print(paste('designType',designType))
      print(paste('orientation',orientation))
      print(paste('lim',lim))
      
      plot_design(dist_intra=d_intra,dist_inter=d_inter,dist_intercrop=dist_intercrop,designType=designType,orientation=orientation,pointSize=5,lim=lim,twist=0)
      
        })
  })
  
  # plot 
  plot<-reactive({
    result<- result()
    if (is.null(result)) return(NULL)
    
   ggplotly(result$plot,height = 1000,width=1000)
  })
  
  # Outputs -----------------------------------------------------------------
  
  ### plot
  output$plot=renderPlotly({plot()})
  
  
  output$ui.action <- renderUI({
    actionButton("action", "Visualize the design")
  })
}

shinyApp(ui, server)




