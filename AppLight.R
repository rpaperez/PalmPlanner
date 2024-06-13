# Load packages -----------------------------------------------------------

packs <- c('shiny','shinythemes','shinycssloaders',"lubridate", "stringr", 'tidyverse','viridis')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack))}
lapply(packs, InstIfNec)


ui<-shinyUI(
  # fluidPage(
  
  # titlePanel("Simulate light transmission"),
  navbarPage(theme = shinytheme("sandstone"),
             title ="Simulating light transmission under oil palm rows",
             sidebarLayout(
               
               sidebarPanel(
                 selectInput(inputId="designType", label = h4("Select design pattern"),
                             choices = list("square" = 'square',
                                            'quincunx'='quincunx',
                                            "square2"="square2",
                                            'quincunx2'='quincunx2',
                                            "square3"="square3",
                                            'quincunx3'='quincunx3',
                                            "square4"="square4",
                                            'quincunx4'='quincunx4',
                                            "square5"="square5",
                                            'quincunx5'='quincunx5')),
                 numericInput("inter_dist",label = "Inter row distance (m):", value=10,step = NA),
                 numericInput("intra_dist",label = "Intra row distance (m):", value=5,step = NA),
                 numericInput("dist_intercrop",label = h4("Intercropping distance (m):"), value=0,step = 0.1,min = 0),
                 # numericInput("MAP",label = "Months after planting:", value=180,step = NA),
                 selectInput(inputId="orientation", label = h4("Select scene orientation"),
                             choices = list("North-South" = 'NS',
                                            "East-West" = 'EW'),
                             selected = 'North-South'),
                 uiOutput('ui.action'),
               ),
               mainPanel(
               
                        plotOutput('plot')%>% withSpinner(color="darkorange"))
                        # plotOutput('plot2')%>% withSpinner(color="darkorange"))
               
             )
  )
)


server<-function(input, output,session){
  
  source('./1-code/helpers_App_Design.R')
  source('./1-code/Mapping_light.R')
  
  
  # paramFileName='Mockup_seed1_MAP_180'
  path_designs='./2-outputs/Run_simu/planting_designs/'
  pathVpalmParam='./2-outputs/Generate_VPalm_param/'
  pathArchimed='./1-code/archimed-phys.jar'
  pathVpalmJar='./1-code/vpalm_biomech.jar'
  pathOpf='./2-outputs/Run_simu/ops/opf/'
  pathOPS='./2-outputs/Run_simu/ops/'
  opfStepExport=6
  
  designType<- reactive({
    input$designType
  })
  
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
  
  # MAP<- reactive({
  #   input$MAP
  # })
  
  # generate the data 
  result<- reactive({
    input$action
    isolate({
      designType=designType()
      d_inter=inter_dist()
      d_intra=intra_dist()
      dist_intercrop=dist_intercrop()
      orientation=orientation()
      
      # MAP=MAP()
      
      if (is.null(d_inter) | is.null(d_intra)) return(NULL)
      
      ## lunch simu
      paramFileName=paste0('Mockup_seed1_MAP_',180)
      
      RunSimu(paramFileName=paramFileName,d_inter=d_inter,d_intra=d_intra,d_intercrop=dist_intercrop,designType=designType,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite=F,orientation=orientation,meteoFile='meteoCampecheFormated.csv',plant_model='elaeis')
      
      #### map

      Create_map(d_inter=d_inter,d_intra=d_intra,d_intercrop=dist_intercrop,designType=designType,path_designs=path_designs,paramFileName=paramFileName,orientation=orientation)
    })
  })
  
  # plot 
  plot<-reactive({
    input$action
    result<- result()
    if (is.null(result)) return(NULL)
    
    result$plot
  })
  
  plot2<-reactive({
    input$action
    result<- result()
    if (is.null(result)) return(NULL)
    
    result$plot2
  })
  
  # Outputs -----------------------------------------------------------------
  
  ### plot
  output$plot=renderPlot({plot()
  })
  
  output$plot2=renderPlot({plot2()
  })
  
  output$ui.action <- renderUI({
    actionButton("action", "Run simulation & visualize the light map")
  })
}

shinyApp(ui, server)
