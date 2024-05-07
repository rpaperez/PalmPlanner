# Load packages -----------------------------------------------------------

packs <- c('shiny','shinythemes','shinycssloaders',"lubridate", "stringr", 'tidyverse','viridis','plotly')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack))}
lapply(packs, InstIfNec)

myTheme=theme_minimal() %+replace% 
  theme( 
    panel.background = element_rect(fill = "white", 
                                    colour = NA), panel.border = element_rect(fill = NA, 
                                                                              colour = "grey20"),
    text = element_text( face = "plain",  size = 14,
                         angle = 0, lineheight = 0.9),
    plot.title = element_text(size = rel(1.2)),
    axis.text = element_text(face = "plain", size = 10)
  )

ui<-shinyUI(
  # fluidPage(
  
  # titlePanel("Simulate light transmission"),
  navbarPage(theme = shinytheme("sandstone"),
             title ="Designing oil palm based intercropping systems",
             
             sidebarLayout( 
               sidebarPanel(
                 tabsetPanel(
                   tabPanel("Palms",
                            selectInput(inputId="designType", label = h4("Select design pattern"),
                                        choices = list("square" = 'square',
                                                       'quincunx'='quincunx',
                                                       "square2" = 'square2',
                                                       'quincunx2'='quincunx2',
                                                       'quincunx3'='quincunx3',
                                                       'quincunx4'='quincunx4',
                                                       'quincunx5'='quincunx5'),
                                        selected = 'quincunx'),
                            numericInput("inter_dist",label = "Inter row distance (m):", value=10,step = 1),
                            numericInput("intra_dist",label = "Intra row distance (m):", value=5,step = 1),
                            numericInput("dist_intercrop",label = "Intercropping distance (m):", value=0,step = 1),
                            
                            # selectInput(inputId="orientation", label = h4("Select scene orientation & plot arguments"),
                            # choices = list("North-South" = 'NS',
                            #                "East-West" = 'EW'),
                            # selected = 'North-South'),
                            numericInput("lim",label = "plot limits (m):", value=50,step = 10),
                            numericInput("pointSize",label = "Select point size:", value=5,step = 1),
                            checkboxInput('origin',label = 'Set origin to the first palm')
                            
                   )
                   ,
                   tabPanel("Intercrop1",
                            numericInput("inter_dist_I1",label = "Inter row distance (m):", value=NULL,step = 1),
                            numericInput("intra_dist_I1",label = "Intra row distance (m):", value=NULL,step = 1),
                            selectInput(inputId="designType_I1", label = h4("Select an design pattern"),
                                        choices = list("square" = 'square',
                                                       'quincunx'='quincunx',
                                                       "square2" = 'square2',
                                                       'quincunx2'='quincunx2',
                                                       'quincunx3'='quincunx3',
                                                       'quincunx4'='quincunx4',
                                                       'quincunx5'='quincunx5'),
                                        selected = 'square')
                   ),
                   tabPanel("Intercrop2",
                            numericInput("inter_dist_I2",label = "Inter row distance (m):", value=NULL,step = 1),
                            numericInput("intra_dist_I2",label = "Intra row distance (m):", value=NULL,step = 1),
                            selectInput(inputId="designType_I2", label = h4("Select an design pattern"),
                                        choices = list("square" = 'square',
                                                       'quincunx'='quincunx',
                                                       "square2" = 'square2',
                                                       'quincunx2'='quincunx2',
                                                       'quincunx3'='quincunx3',
                                                       'quincunx4'='quincunx4',
                                                       'quincunx5'='quincunx5'),
                                        selected = 'square')
                   )
                 ),
                 
                 uiOutput('ui.action'),
               ),
               mainPanel(
                 
                 plotlyOutput('plot')%>% withSpinner(color="darkorange"))
               
             )
  )
)




server<-function(input, output,session){
  
  source('./1-code/helpers_App_Design.R')
  
  ##plam parameters
  inter_dist<- reactive({
    input$inter_dist
  })
  
  intra_dist<- reactive({
    input$intra_dist
  })
  
  dist_intercrop<- reactive({
    input$dist_intercrop
  })
  
  
  # orientation<- reactive({
  #   input$orientation
  # })
  
  designType<- reactive({
    input$designType
  })
  
  lim<- reactive({
    input$lim
  })
  
  pointSize<- reactive({
    input$pointSize
  })
  
  origin<- reactive({
    input$origin
  })
  
  ### intercrop1 parameters
  
  designType_I1<- reactive({
    input$designType_I1
  })
  inter_dist_I1<- reactive({
    input$inter_dist_I1
  })
  
  intra_dist_I1<- reactive({
    input$intra_dist_I1
  })
  
  ### intercrop2 parameters
  
  designType_I2<- reactive({
    input$designType_I2
  })
  inter_dist_I2<- reactive({
    input$inter_dist_I2
  })
  
  intra_dist_I2<- reactive({
    input$intra_dist_I2
  })
  
  
  
  # generate the data 
  result<- reactive({
    input$action
    isolate({
      d_inter=inter_dist()
      d_intra=intra_dist()
      dist_intercrop=dist_intercrop()
      # orientation=orientation()
      orientation='NS'
      designType=designType()
      lim=lim()
      pointSize=pointSize()
      origin=origin()
      
      inter_dist_I1=inter_dist_I1()
      intra_dist_I1=intra_dist_I1()
      designType_I1=designType_I1()
      
      
      inter_dist_I2=inter_dist_I2()
      intra_dist_I2=intra_dist_I2()
      designType_I2=designType_I2()
      
      
      if (is.null(d_inter) | is.null(d_intra)) {
        return(NULL)
      }
      
      ## lunch simu
      
      #### map
      # print(paste('dist_intra',d_intra))
      # print(paste('dist_inter',d_inter))
      # print(paste('dist_intercrop',dist_intercrop))
      # print(paste('designType',designType))
      # print(paste('orientation',orientation))
      # print(paste('lim',lim))
      
      design=plot_design(dist_intra=d_intra,dist_inter=d_inter,dist_intercrop=dist_intercrop,designType=designType,orientation=orientation,pointSize=pointSize,lim=lim,twist=0)
      
      if(origin==T){
        design$design$x=design$design$x-design$design$x[1]
        design$design$y=design$design$y-design$design$y[1]
      }
      
        gr=ggplot()+
          geom_point(data=design$design,aes(x=x,y=y,col= 'palms'),shape=8,size=pointSize)+
          xlim(c(0,lim))+
          ylim(c(0,lim))+
          labs(col=NULL)+
          ggtitle(paste(design$density,' plants.ha-1'))+
          coord_equal()+
          myTheme
     

      
      
      
      if ( !is.na(intra_dist_I1) & !is.na(inter_dist_I1) & is.na(intra_dist_I2) & is.na(inter_dist_I2)){
        
        # print(paste('_____inter_dist_I1',inter_dist_I1))
        # print(paste('_____intra_dist_I1',intra_dist_I1))
        # print(paste('_____designType_I1',designType_I1))
        
        I1=design_intercrop(dist_intra =d_intra,dist_inter =d_inter,designType =designType,dist_intercrop =dist_intercrop,orientation = orientation,twist = 0,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I1,I_dist_inter =inter_dist_I1,I_designType = designType_I1)
        

        
        if(origin==T){
          x_offset=I1$designPalm$x[1]
          y_offset=I1$designPalm$y[1]
          I1$designPalm$x=I1$designPalm$x-x_offset
          I1$designPalm$y=I1$designPalm$y-y_offset
          I1$designI$x=I1$designI$x-x_offset
          I1$designI$y=I1$designI$y-y_offset
        }
        
        gr=ggplot()+
          geom_point(data=I1$designPalm,aes(x=x,y=y,col= 'palms'),shape=8,size=pointSize)+
          geom_point(data=I1$designI,aes(x=x,y=y,col='intercrop1'),size=max(1,pointSize-2),alpha=0.8)+
          xlim(c(0,lim))+
          ylim(c(0,lim))+
          labs(col=NULL)+
          ggtitle(paste(design$density,' plants.ha-1'))+
          coord_equal()+
          myTheme
        
      }
      
      if ( !is.na(intra_dist_I2) & !is.na(inter_dist_I2) & is.na(intra_dist_I1) & is.na(inter_dist_I1)){
        
        # print(paste('_____inter_dist_I1',inter_dist_I1))
        # print(paste('_____intra_dist_I1',intra_dist_I1))
        # print(paste('_____designType_I1',designType_I1))
        
        I2=design_intercrop(dist_intra =d_intra,dist_inter =d_inter,designType =designType,dist_intercrop =dist_intercrop,orientation = orientation,twist = 0,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I2,I_dist_inter =inter_dist_I2,I_designType = designType_I2)
        
        if(origin==T){
          x_offset=I2$designPalm$x[1]
          y_offset=I2$designPalm$y[1]
          
          I2$designPalm$x=I2$designPalm$x-x_offset
          I2$designPalm$y=I1$designPalm$y-y_offset
          I2$designI$x=I2$designI$x-x_offset
          I2$designI$y=I2$designI$y-y_offset
        }
        
        gr=ggplot()+
          geom_point(data=I1$designPalm,aes(x=x,y=y,col= 'palms'),shape=8,size=pointSize)+
          geom_point(data=I1$designI,aes(x=x,y=y,col='intercrop2'),size=max(1,pointSize-2),alpha=0.8,pch=17)+
          xlim(c(0,lim))+
          ylim(c(0,lim))+
          labs(col=NULL)+
          ggtitle(paste(design$density,' plants.ha-1'))+
          coord_equal()+
          myTheme
        
      }
      
      if (!is.na(intra_dist_I1) & !is.na(inter_dist_I1) & !is.na(intra_dist_I2) & !is.na(inter_dist_I2)){
        
        I1=design_intercrop(dist_intra =d_intra,dist_inter =d_inter,designType =designType,dist_intercrop =dist_intercrop,orientation = orientation,twist = 0,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I1,I_dist_inter =inter_dist_I1,I_designType = designType_I1)
        
        I2=design_intercrop(dist_intra =d_intra,dist_inter =d_inter,designType =designType,dist_intercrop =dist_intercrop,orientation = orientation,twist = 0,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I2,I_dist_inter =inter_dist_I2,I_designType = designType_I2)
        
        if(origin==T){
          x_offset=I1$designPalm$x[1]
          y_offset=I1$designPalm$y[1]
          
          I1$designPalm$x=I1$designPalm$x-x_offset
          I1$designPalm$y=I1$designPalm$y-y_offset
          I1$designI$x=I1$designI$x-x_offset
          I1$designI$y=I1$designI$y-y_offset
          
          I2$designI$x=I2$designI$x-x_offset
          I2$designI$y=I2$designI$y-y_offset
        }
        
        gr=ggplot()+
          geom_point(data=I1$designPalm,aes(x=x,y=y,col= 'palms'),shape=8,size=pointSize)+
          geom_point(data=I1$designI,aes(x=x,y=y,col='intercrop1'),size=max(1,pointSize-2),alpha=0.8)+
          geom_point(data=I2$designI,aes(x=x,y=y,col='intercrop2'),size=max(1,pointSize-2),pch=17,alpha=0.8)+
          xlim(c(0,lim))+
          ylim(c(0,lim))+
          labs(col=NULL)+
          ggtitle(paste(design$density,' plants.ha-1'))+
          coord_equal()+
          myTheme
        
      }
      
      gr
      
      
    })
  })
  
  # plot 
  plot<-reactive({
    result<- result()
    if (is.null(result)) return(NULL)
    
    ggplotly(result,height = 800,width=800)
  })
  
  # Outputs -----------------------------------------------------------------
  
  ### plot
  output$plot=renderPlotly({plot()})
  
  
  output$ui.action <- renderUI({
    actionButton("action", "Visualize the design")
  })
}

shinyApp(ui, server)




