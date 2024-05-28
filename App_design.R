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

tableDesign=data.frame(designRef=c('square','quincunx',"square2",'quincunx2',"square3",'quincunx3',"square4",'quincunx4',"square5",'quincunx5'),
                       design=c('square','quincunx','square','quincunx','square','quincunx','square','quincunx','square','quincunx'),
                       NbR=c('0','0',"1/3","1/3","1/4","1/4","1/5","1/5","1/6","1/6"),
                       NbLines=c('1','1','2','2','3','3','4','4','5','5'))

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
                                                       'quincunx'='quincunx'),
                                        selected = 'quincunx'),
                            selectInput(inputId="NbRemoved", label = h4("Number of lines removed for intercropping"),
                                        choices = list('0'='0',
                                                       "1/3" = '1/3',
                                                       "1/4"='1/4',
                                                       "1/5"='1/5',
                                                       "1/6"='1/6'),
                                        selected = '0'),      
                            
                            
                            numericInput("inter_dist",label = "Inter palm row distance (m):", value=10,step = 1,min = 0),
                            numericInput("intra_dist",label = "Intra palm row distance (m):", value=5,step = 1,min = 0),
                            numericInput("dist_intercrop",label = "Intercropping distance (m):", value=0,step = 1,min = 0),
                            
                            # selectInput(inputId="orientation", label = h4("Select scene orientation & plot arguments"),
                            # choices = list("North-South" = 'NS',
                            #                "East-West" = 'EW'),
                            # selected = 'North-South'),
                            numericInput("pointSize",label = "Select point size:", value=5,step = 1,min = 0),
                            colourInput("colPalm", "Select colour", "forestgreen"),
                            numericInput("lim",label = "plot limits (m):", value=50,step = 10,min = 0),
                            checkboxInput('origin',label = 'Set origin to the first palm')
                            
                   )
                   ,
                   tabPanel("Intercrop1",
                            textInput(inputId = "Int1", label = "Enter the name of the intercrop",value = 'intercrop 1'),
                            numericInput("inter_dist_I1",label = "Inter row distance (m):", value=NULL,step = 1,min = 0),
                            numericInput("intra_dist_I1",label = "Intra row distance (m):", value=NULL,step = 1,min = 0),
                            selectInput(inputId="designType_I1", label = h4("Select design pattern"),
                                        choices = list("square" = 'square',
                                                       'quincunx'='quincunx'),
                                        selected = 'quincunx'),
                            selectInput(inputId="NbLines_I1", label = h4("Number of lines"),
                                        choices = list('1'='1',
                                                       "2" = '2',
                                                       "3"='3',
                                                       "4"='4',
                                                       "5"='5'),
                                        selected = '1'),  
                            numericInput("pointSizeInt1",label = "Select point size:", value=2,step = 1,min = 0),
                            colourInput("colInt1", "Select colour", "orange"),
                            
                   ),
                   tabPanel("Intercrop2",
                            textInput(inputId = "Int2",label =  "Enter the name of the intercrop",value = 'intercrop 2'),
                            numericInput("inter_dist_I2",label = "Inter row distance (m):", value=NULL,step = 1,min = 0),
                            numericInput("intra_dist_I2",label = "Intra row distance (m):", value=NULL,step = 1,min = 0),
                            selectInput(inputId="designType_I2", label = h4("Select design pattern"),
                                        choices = list("square" = 'square',
                                                       'quincunx'='quincunx'),
                                        selected = 'quincunx'),
                            selectInput(inputId="NbLines_I2", label = h4("Number of lines"),
                                        choices = list('1'='1',
                                                       "2" = '2',
                                                       "3"='3',
                                                       "4"='4',
                                                       "5"='5'),
                                        selected = '1'),    
                            numericInput("pointSizeInt2",label = "Select point size:", value=2,step = 1,min = 0),
                            colourInput("colInt2", "Select colour", "red"),
                            
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
  
  NbRemoved<- reactive({
    input$NbRemoved
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
  NbLines_I1<- reactive({
    input$NbLines_I1
  })
  
  inter_dist_I1<- reactive({
    input$inter_dist_I1
  })
  
  intra_dist_I1<- reactive({
    input$intra_dist_I1
  })
  
  Int1<- reactive({
    input$Int1
  })
  
  ### intercrop2 parameters
  
  designType_I2<- reactive({
    input$designType_I2
  })
  
  NbLines_I2<- reactive({
    input$NbLines_I2
  })
  
  inter_dist_I2<- reactive({
    input$inter_dist_I2
  })
  
  intra_dist_I2<- reactive({
    input$intra_dist_I2
  })
  
  Int2<- reactive({
    input$Int2
  })
  
  
  # generate the data 
  result<- reactive({
    input$action
    
    ## imposed intercropping distance for specific design
    if(  input$NbRemoved=='0' &  input$dist_intercrop!=0){
      updateNumericInput(session, "dist_intercrop", value = 0)
    }
    
    if(  input$NbRemoved!='0' &  input$dist_intercrop==0){
      updateNumericInput(session, "dist_intercrop", value = 2*input$inter_dist)
    }
    
    
    isolate({
      d_inter=inter_dist()
      d_intra=intra_dist()
      dist_intercrop=dist_intercrop()
      # orientation=orientation()
      orientation='NS'
      designType=designType()
      NbRemoved=NbRemoved()
      lim=lim()
      pointSize=pointSize()
      origin=origin()
      
      inter_dist_I1=inter_dist_I1()
      intra_dist_I1=intra_dist_I1()
      designType_I1=designType_I1()
      NbLines_I1=NbLines_I1()
      Int1=Int1()
      
      inter_dist_I2=inter_dist_I2()
      intra_dist_I2=intra_dist_I2()
      designType_I2=designType_I2()
      NbLines_I2=NbLines_I2()
      Int2=Int2()
      
      if (is.null(d_inter) | is.null(d_intra)) {
        return(NULL)
      }
      
      ## lunch simu
      
      tableColor=c(input$colPalm,input$colInt1,input$colInt2)
      
      
      #### map
      # print(paste('dist_intra',d_intra))
      # print(paste('dist_inter',d_inter))
      # print(paste('dist_intercrop',dist_intercrop))
      # print(paste('designType',designType))
      # print(paste('orientation',orientation))
      # print(paste('lim',lim))
      designRef=tableDesign %>% filter(design==designType & NbR==NbRemoved) %>% select(designRef)
      
      design=plot_design(dist_intra=d_intra,dist_inter=d_inter,dist_intercrop=dist_intercrop,designType=designRef[[1]],orientation=orientation,pointSize=pointSize,lim=lim,twist=0)
      
      if(origin==T){
        design$design$x=design$design$x-design$design$x[1]
        design$design$y=design$design$y-design$design$y[1]
      }
      
      names1=paste0(design$density,' palms.ha-1')
      names(tableColor)=c(names1,Int1,Int2)
      
      gr=ggplot()+
        geom_point(data=design$design,aes(x=x,y=y,col= names1),shape=8,size=pointSize)+
        xlim(c(0,lim))+
        ylim(c(0,lim))+
        labs(col=NULL)+
        coord_equal()+
        scale_color_manual(values = tableColor)+
        myTheme
      
      
      
      
      
      if ( !is.na(intra_dist_I1) & !is.na(inter_dist_I1) & is.na(intra_dist_I2) & is.na(inter_dist_I2)){
        designRef_I1=tableDesign %>% filter(design==designType_I1 & NbLines==NbLines_I1) %>% select(designRef)
        
        
        I1=design_intercrop(dist_intra =d_intra,dist_inter =d_inter,designType =designRef[[1]],dist_intercrop =dist_intercrop,orientation = orientation,twist = 0,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I1,I_dist_inter =inter_dist_I1,I_designType = designRef_I1[[1]])
        
        
        
        
        if(origin==T){
          x_offset=I1$designPalm$x[1]
          y_offset=I1$designPalm$y[1]
          I1$designPalm$x=I1$designPalm$x-x_offset
          I1$designPalm$y=I1$designPalm$y-y_offset
          I1$designI$x=I1$designI$x-x_offset
          I1$designI$y=I1$designI$y-y_offset
        }
        
        names1=paste0(design$density,' palms.ha-1')
        names2=paste0(I1$density,' ',Int1,'.ha-1')
        names(tableColor)=c(names1,names2,Int2)
        
        
        gr=ggplot()+
          geom_point(data=I1$designPalm,aes(x=x,y=y,col= names1),shape=8,size=pointSize)+
          geom_point(data=I1$designI,aes(x=x,y=y,col=names2),size=input$pointSizeInt1,alpha=0.8)+
          xlim(c(0,lim))+
          ylim(c(0,lim))+
          labs(col=NULL)+
          scale_color_manual(values = tableColor)+
          coord_equal()+
          myTheme
        
      }
      
      if ( !is.na(intra_dist_I2) & !is.na(inter_dist_I2) & is.na(intra_dist_I1) & is.na(inter_dist_I1)){
        
        # print(paste('_____inter_dist_I1',inter_dist_I1))
        # print(paste('_____intra_dist_I1',intra_dist_I1))
        # print(paste('_____designType_I1',designType_I1))
        designRef_I2=tableDesign %>% filter(design==designType_I2 & NbLines==NbLines_I2) %>% select(designRef)
        
        
        I2=design_intercrop(dist_intra =d_intra,dist_inter =d_inter,designType =designRef[[1]],dist_intercrop =dist_intercrop,orientation = orientation,twist = 0,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I2,I_dist_inter =inter_dist_I2,I_designType = designRef_I2[[1]])
        
        if(origin==T){
          x_offset=I2$designPalm$x[1]
          y_offset=I2$designPalm$y[1]
          
          I2$designPalm$x=I2$designPalm$x-x_offset
          I2$designPalm$y=I1$designPalm$y-y_offset
          I2$designI$x=I2$designI$x-x_offset
          I2$designI$y=I2$designI$y-y_offset
        }
        
        names1=paste0(design$density,' palms.ha-1')
        names3=paste0(I2$density,' ',Int2,'.ha-1')
        names(tableColor)=c(names1,names2,names3)
        
        gr=ggplot()+
          geom_point(data=I1$designPalm,aes(x=x,y=y,col= names1),shape=8,size=pointSize)+
          geom_point(data=I1$designI,aes(x=x,y=y,col=names3),size=input$pointSizeInt1,alpha=0.8,pch=15)+
          xlim(c(0,lim))+
          ylim(c(0,lim))+
          labs(col=NULL)+
          scale_color_manual(values = tableColor)+
          coord_equal()+
          myTheme
        
      }
      
      if (!is.na(intra_dist_I1) & !is.na(inter_dist_I1) & !is.na(intra_dist_I2) & !is.na(inter_dist_I2)){
        
        designRef_I1=tableDesign %>% filter(design==designType_I1 & NbLines==NbLines_I1) %>% select(designRef)
        designRef_I2=tableDesign %>% filter(design==designType_I2 & NbLines==NbLines_I2) %>% select(designRef)
        
        
        I1=design_intercrop(dist_intra =d_intra,dist_inter =d_inter,designType =designRef[[1]],dist_intercrop =dist_intercrop,orientation = orientation,twist = 0,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I1,I_dist_inter =inter_dist_I1,I_designType = designRef_I1[[1]])
        
        I2=design_intercrop(dist_intra =d_intra,dist_inter =d_inter,designType =designRef[[1]],dist_intercrop =dist_intercrop,orientation = orientation,twist = 0,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I2,I_dist_inter =inter_dist_I2,I_designType = designRef_I2[[1]])
        
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
        
        names1=paste0(design$density,' palms.ha-1')
        names2=paste0(I1$density,' ',Int1,'.ha-1')
        names3=paste0(I2$density,' ',Int2,'.ha-1')
        names(tableColor)=c(names1,names2,names3)
        
        
        gr=ggplot()+
          geom_point(data=I1$designPalm,aes(x=x,y=y,col= names1),shape=8,size=pointSize)+
          geom_point(data=I1$designI,aes(x=x,y=y,col=names2),size=input$pointSizeInt1,alpha=0.8)+
          geom_point(data=I2$designI,aes(x=x,y=y,col=names3),size=input$pointSizeInt2,pch=15,alpha=0.8)+
          xlim(c(0,lim))+
          ylim(c(0,lim))+
          labs(col=NULL)+
          scale_color_manual(values = tableColor)+
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




