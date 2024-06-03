# Load packages -----------------------------------------------------------




packs <- c('shiny','shinythemes','shinycssloaders',"lubridate", "stringr", 'tidyverse','viridis','plotly','devtools')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack))}
lapply(packs, InstIfNec)

if (!do.call(require,list('colourpicker'))) {
  devtools::install_github("daattali/colourpicker")
}

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
                       NbLines=c(1,1,2,2,3,3,4,4,5,5))

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
                            
                            numericInput("inter_dist",label = h4("Inter palm row distance (m):"), value=10,step = 0.1,min = 0),
                            numericInput("intra_dist",label = h4("Intra palm row distance (m):"), value=5,step = 0.1,min = 0),
                            numericInput("dist_intercrop",label = h4("Intercropping distance (m):"), value=0,step = 0.1,min = 0),
                            
                            # selectInput(inputId="orientation", label = h4("Select scene orientation & plot arguments"),
                            # choices = list("North-South" = 'NS',
                            #                "East-West" = 'EW'),
                            # selected = 'North-South'),
                            tags$hr(),
                            numericInput("pointSize",label = h4("Select point size:"), value=5,step = 0.1,min = 0),
                            colourInput("colPalm", h4("Select colour"), "forestgreen")
                            
                   )
                   ,
                   tabPanel("Intercrop1",
                            textInput(inputId = "Int1", label = h4("Enter the name of the intercrop"),value = 'intercrop 1'),
                            selectInput(inputId="designType_I1", label = h4("Select design pattern"),
                                        choices = list("square" = 'square',
                                                       'quincunx'='quincunx'),
                                        selected = 'square'),
                            selectInput(inputId="NbLines_I1", label = h4("Number of lines"),
                                        choices = list('0'=0,
                                                       '1'=1,
                                                       "2" =2,
                                                       "3"=3,
                                                       "4"=4,
                                                       "5"=5),
                                        selected = 0),  
                            numericInput("inter_dist_I1",label = h4("Inter row distance (m):"), value=NULL,step = 0.1,min = 0),
                            numericInput("intra_dist_I1",label = h4("Intra row distance (m):"), value=NULL,step = 0.1,min = 0),
                            numericInput("offset_Y_I1",label = h4("y offset (m)"), value=0,step = 0.05,max=0),
                            tags$hr(),
                            numericInput("pointSizeInt1",label = h4("Select point size:"), value=2,step = 0.1,min = 0),
                            colourInput("colInt1", h4("Select colour"), "orange"),
                            
                   ),
                   tabPanel("Intercrop2",
                            textInput(inputId = "Int2",label =  h4("Enter the name of the intercrop"),value = 'intercrop 2'),
                            selectInput(inputId="designType_I2", label = h4("Select design pattern"),
                                        choices = list("square" = 'square',
                                                       'quincunx'='quincunx'),
                                        selected = 'square'),
                            selectInput(inputId="NbLines_I2", label = h4("Number of lines"),
                                        choices = list('0'=0,
                                                       '1'=1,
                                                       "2" = 2,
                                                       "3"=3,
                                                       "4"=4,
                                                       "5"=5),
                                        selected =0), 
                            numericInput("inter_dist_I2",label = h4("Inter row distance (m):"), value=NULL,step = 0.1,min = 0),
                            numericInput("intra_dist_I2",label = h4("Intra row distance (m):"), value=NULL,step = 0.1,min = 0),
                            numericInput("offset_Y_I2",label = h4("y offset (m)"), value=0,step = 0.05),
                            tags$hr(),
                            numericInput("pointSizeInt2",label = h4("Select point size:"), value=2,step = 0.1,min = 0),
                            colourInput("colInt2", h4("Select colour"), "red"),
                            
                   ),
                   tabPanel("Intercrop3",
                            textInput(inputId = "Int3",label =  h4("Enter the name of the intercrop"),value = 'intercrop 3'),
                            selectInput(inputId="designType_I3", label = h4("Select design pattern"),
                                        choices = list("square" = 'square',
                                                       'quincunx'='quincunx'),
                                        selected = 'square'),
                            selectInput(inputId="NbLines_I3", label = h4("Number of lines"),
                                        choices = list('0'=0,
                                                       '1'=1,
                                                       "2" = 2,
                                                       "3"=3,
                                                       "4"=4,
                                                       "5"=5),
                                        selected =0), 
                            numericInput("inter_dist_I3",label = h4("Inter row distance (m):"), value=NULL,step = 0.1,min = 0),
                            numericInput("intra_dist_I3",label = h4("Intra row distance (m):"), value=NULL,step = 0.1,min = 0),
                            numericInput("offset_Y_I3",label = h4("y offset (m)"), value=0,step = 0.05),
                            tags$hr(),
                            numericInput("pointSizeInt3",label = h4("Select point size:"), value=2,step = 0.1,min = 0),
                            colourInput("colInt3", h4("Select colour"), "purple"),
                            
                   ),
                   
                 ),
                 tags$hr(),
                 tags$hr(),
                 numericInput("lim",label = h4("plot limits (m):"), value=50,step = 10,min = 0),
                 checkboxInput('origin',label = 'Set origin to the first palm',value = T),
                 uiOutput('ui.action')
                 
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
  
  offset_Y_I1<- reactive({
    input$offset_Y_I1
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
  
  offset_Y_I2<- reactive({
    input$offset_Y_I2
  })
  
  ### intercrop3 parameters
  
  designType_I3<- reactive({
    input$designType_I3
  })
  NbLines_I3<- reactive({
    input$NbLines_I3
  })
  
  inter_dist_I3<- reactive({
    input$inter_dist_I3
  })
  
  intra_dist_I3<- reactive({
    input$intra_dist_I3
  })
  
  Int3<- reactive({
    input$Int3
  })
  
  offset_Y_I3<- reactive({
    input$offset_Y_I3
  })
  
  ## condition
  
  cond<- reactive({
    
    observe({
      ## imposed intercropping distance for specific design
      if(  input$NbRemoved=='0' &  input$dist_intercrop!=0){
        updateNumericInput(session, "dist_intercrop", value = 0)
      }
      
      if(  input$NbRemoved!='0' &  input$dist_intercrop==0){
        updateNumericInput(session, "dist_intercrop", value = 2*input$inter_dist)
      }
      
      # inactivation of parameters if no intercorp 
      
      if(input$NbLines_I1==0){
        updateNumericInput(session, "inter_dist_I1", value = NA)
        updateNumericInput(session, "intra_dist_I1", value = NA)
      }
      
      if(  input$NbLines_I2==0){
        updateNumericInput(session, "inter_dist_I2", value = NA)
        updateNumericInput(session, "intra_dist_I2", value = NA)
      }
      
      if(  input$NbLines_I3==0){
        updateNumericInput(session, "inter_dist_I3", value = NA)
        updateNumericInput(session, "intra_dist_I3", value = NA)
      }
      
      # inactivation of inter row distance for 1 row
      
      if(  input$NbLines_I1==1 & !is.na(input$inter_dist_I1)){
        updateNumericInput(session, "inter_dist_I1", value = 0)
      }
      if(  input$NbLines_I1==1 & is.na(input$intra_dist_I1) | input$NbLines_I1==1 & input$intra_dist_I1==0){   updateNumericInput(session, "intra_dist_I1", value = 1)
      }
      
      if(input$NbLines_I2==1 &  !is.na(input$inter_dist_I2)){
        updateNumericInput(session, "inter_dist_I2", value = 0)
      }
      if(  input$NbLines_I2==1 & is.na(input$intra_dist_I2)|input$NbLines_I2==1 & input$intra_dist_I2==0){
        updateNumericInput(session, "intra_dist_I2", value = 1)
      }
      
      if(input$NbLines_I3==1 &  !is.na(input$inter_dist_I3)){
        updateNumericInput(session, "inter_dist_I3", value = 0)
      }
      if(  input$NbLines_I3==1 & is.na(input$intra_dist_I3)|input$NbLines_I3==1 & input$intra_dist_I3==0){
        updateNumericInput(session, "intra_dist_I3", value = 1)
      }
      
      ## avoid inter row null when multiple row design is selected
      
      if(  input$NbLines_I1>1 & is.na(input$inter_dist_I1) | input$NbLines_I1>1 & !is.na(input$inter_dist_I1) & input$inter_dist_I1==0){
        updateNumericInput(session, "inter_dist_I1", value = 1)
      }
      if(  input$NbLines_I1>1 & is.na(input$intra_dist_I1) | input$NbLines_I1>1 & !is.na(input$intra_dist_I1) & input$intra_dist_I1==0){
        updateNumericInput(session, "intra_dist_I1", value = 1)
      }
      
      if(  input$NbLines_I2>1 &  is.na(input$inter_dist_I2)| input$NbLines_I2>1 & input$inter_dist_I2==0){
        updateNumericInput(session, "inter_dist_I2", value = 1)
      }
      if(  input$NbLines_I2>1 & is.na(input$intra_dist_I2) | input$NbLines_I2>1 & input$intra_dist_I2==0){      updateNumericInput(session, "intra_dist_I2", value = 1)
      }
      
      if(  input$NbLines_I3>1 &  is.na(input$inter_dist_I3)| input$NbLines_I3>1 & input$inter_dist_I3==0){
        updateNumericInput(session, "inter_dist_I3", value = 1)
      }
      if(  input$NbLines_I3>1 & is.na(input$intra_dist_I3) | input$NbLines_I3>1 & input$intra_dist_I3==0){      updateNumericInput(session, "intra_dist_I3", value = 1)
      }
      
    })
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
      NbRemoved=NbRemoved()
      lim=lim()
      pointSize=pointSize()
      origin=origin()
      
      inter_dist_I1=inter_dist_I1()
      intra_dist_I1=intra_dist_I1()
      designType_I1=designType_I1()
      NbLines_I1=NbLines_I1()
      Int1=Int1()
      offset_Y_I1=offset_Y_I1()
      
      inter_dist_I2=inter_dist_I2()
      intra_dist_I2=intra_dist_I2()
      designType_I2=designType_I2()
      NbLines_I2=NbLines_I2()
      Int2=Int2()
      offset_Y_I2=offset_Y_I2()
      
      inter_dist_I3=inter_dist_I3()
      intra_dist_I3=intra_dist_I3()
      designType_I3=designType_I3()
      NbLines_I3=NbLines_I3()
      Int3=Int3()
      offset_Y_I3=offset_Y_I3()
      
      if (is.null(d_inter) | is.null(d_intra)) {
        return(NULL)
      }
      
      ## lunch simu
      
      tableColor=c(input$colPalm,input$colInt1,input$colInt2,input$colInt3)
      
      
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
        x_offset=design$design$x[1]
        y_offset=design$design$y[1]
        design$design$x=design$design$x-x_offset
        design$design$y=design$design$y-y_offset
      }
      
      Id=paste0(design$density,' palms.ha-1')
      names(tableColor)=c(Id,Int1,Int2,Int3)
      
      gr=ggplot()+
        geom_point(data=design$design,aes(x=x,y=y,col= Id),shape=8,size=pointSize)+
        xlim(c(-1,lim))+
        ylim(c(-1,lim))+
        labs(col=NULL)+
        coord_equal()+
        scale_color_manual(values = tableColor)+
        myTheme
      
      
      ### with intercrops####
      if ( NbLines_I1>0){
        designRef_I1=tableDesign %>% filter(design==designType_I1 & NbLines==NbLines_I1) %>% select(designRef)
        
        I1=design_intercrop(dist_intra =d_intra,dist_inter =d_inter,designType =designRef[[1]],dist_intercrop =dist_intercrop,orientation = orientation,twist = 0,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I1,I_dist_inter =inter_dist_I1,I_designType = designRef_I1[[1]])
        
        if(origin==T){
          # x_offset=I1$designPalm$x[1]
          # y_offset=I1$designPalm$y[1]
          I1$designPalm$x=I1$designPalm$x-x_offset
          I1$designPalm$y=I1$designPalm$y-y_offset
          I1$designI$x=I1$designI$x-x_offset
          I1$designI$y=I1$designI$y-y_offset
        }
        
        I1$designI$y= I1$designI$y+offset_Y_I1
      }
      
      if ( NbLines_I2>0){
        designRef_I2=tableDesign %>% filter(design==designType_I2 & NbLines==NbLines_I2) %>% select(designRef)
        
        I2=design_intercrop(dist_intra =d_intra,dist_inter =d_inter,designType =designRef[[1]],dist_intercrop =dist_intercrop,orientation = orientation,twist = 0,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I2,I_dist_inter =inter_dist_I2,I_designType = designRef_I2[[1]])
        
        if(origin==T){
          # x_offset=I2$designPalm$x[1]
          # y_offset=I2$designPalm$y[1]
          
          I2$designPalm$x=I2$designPalm$x-x_offset
          I2$designPalm$y=I2$designPalm$y-y_offset
          I2$designI$x=I2$designI$x-x_offset
          I2$designI$y=I2$designI$y-y_offset
        }
        
        I2$designI$y= I2$designI$y+offset_Y_I2
      }
      
      
      if ( NbLines_I3>0){
        designRef_I3=tableDesign %>% filter(design==designType_I3 & NbLines==NbLines_I3) %>% select(designRef)
        
        
        I3=design_intercrop(dist_intra =d_intra,dist_inter =d_inter,designType =designRef[[1]],dist_intercrop =dist_intercrop,orientation = orientation,twist = 0,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I3,I_dist_inter =inter_dist_I3,I_designType = designRef_I3[[1]])
        
        if(origin==T){
          # x_offset=I3$designPalm$x[1]
          # y_offset=I3$designPalm$y[1]
          
          I3$designPalm$x=I3$designPalm$x-x_offset
          I3$designPalm$y=I3$designPalm$y-y_offset
          I3$designI$x=I3$designI$x-x_offset
          I3$designI$y=I3$designI$y-y_offset
        }
        I3$designI$y= I3$designI$y+offset_Y_I3
        
      }
      
      
      ###graph intercrops #####
      ### intercrop 1 alone####
      
      if ( NbLines_I1>0 & NbLines_I2==0 & NbLines_I3==0){
        
        Id=paste0(design$density,' palms.ha-1')
        Id1=paste0(I1$density,' ',Int1,'.ha-1')
        names(tableColor)=c(Id,Id1,Int2,Int3)
        
        
        gr=ggplot()+
          geom_point(data=I1$designPalm,aes(x=x,y=y,col= Id),shape=8,size=pointSize)+
          geom_point(data=I1$designI,aes(x=x,y=y,col=Id1),size=input$pointSizeInt1,alpha=0.8)+
          xlim(c(-1,lim))+
          ylim(c(-1,lim))+
          labs(col=NULL)+
          scale_color_manual(values = tableColor)+
          coord_equal()+
          myTheme
        
      }
      
      if ( NbLines_I2>0 & NbLines_I1==0 & NbLines_I3==0){
        Id=paste0(design$density,' palms.ha-1')
        Id2=paste0(I2$density,' ',Int2,'.ha-1')
        names(tableColor)=c(Id,Int1,Id2,Int3)
        
        gr=ggplot()+
          geom_point(data=I2$designPalm,aes(x=x,y=y,col= Id),shape=8,size=pointSize)+
          geom_point(data=I2$designI,aes(x=x,y=y,col=Id2),size=input$pointSizeInt2,alpha=0.8,pch=15)+
          xlim(c(-1,lim))+
          ylim(c(-1,lim))+
          labs(col=NULL)+
          scale_color_manual(values = tableColor)+
          coord_equal()+
          myTheme
        
      }
      if ( NbLines_I3>0 & NbLines_I1==0 & NbLines_I2==0){
        Id=paste0(design$density,' palms.ha-1')
        Id3=paste0(I3$density,' ',Int3,'.ha-1')
        names(tableColor)=c(Id,Int1,Int2,Id3)
        
        gr=ggplot()+
          geom_point(data=I3$designPalm,aes(x=x,y=y,col= Id),shape=8,size=pointSize)+
          geom_point(data=I3$designI,aes(x=x,y=y,col=Id3),size=input$pointSizeInt3,alpha=0.8,pch=18)+
          xlim(c(-1,lim))+
          ylim(c(-1,lim))+
          labs(col=NULL)+
          scale_color_manual(values = tableColor)+
          coord_equal()+
          myTheme
        
      }
      
      # ###intercrop 1+ intercrop 2 #####
      if ( NbLines_I2>0 & NbLines_I1>0 & NbLines_I3==0){
        
        Id=paste0(design$density,' palms.ha-1')
        Id1=paste0(I1$density,' ',Int1,'.ha-1')
        Id2=paste0(I2$density,' ',Int2,'.ha-1')
        names(tableColor)=c(Id,Id1,Id2,Int3)
        
        
        gr=ggplot()+
          geom_point(data=I1$designPalm,aes(x=x,y=y,col= Id),shape=8,size=pointSize)+
          geom_point(data=I1$designI,aes(x=x,y=y,col=Id1),size=input$pointSizeInt1,alpha=0.6)+
          geom_point(data=I2$designI,aes(x=x,y=y,col=Id2),size=input$pointSizeInt2,pch=15,alpha=0.6)+
          xlim(c(-1,lim))+
          ylim(c(-1,lim))+
          labs(col=NULL)+
          scale_color_manual(values = tableColor)+
          coord_equal()+
          myTheme
        
      }
      
      ###intercrop 1+ intercrop 3 #####
      if ( NbLines_I3>0 & NbLines_I1>0 & NbLines_I2==0){
        
        Id=paste0(design$density,' palms.ha-1')
        Id1=paste0(I1$density,' ',Int1,'.ha-1')
        Id3=paste0(I3$density,' ',Int3,'.ha-1')
        names(tableColor)=c(Id,Id1,Int2,Id3)
        
        
        gr=ggplot()+
          geom_point(data=I1$designPalm,aes(x=x,y=y,col= Id),shape=8,size=pointSize)+
          geom_point(data=I1$designI,aes(x=x,y=y,col=Id1),size=input$pointSizeInt1,alpha=0.6)+
          geom_point(data=I3$designI,aes(x=x,y=y,col=Id3),size=input$pointSizeInt3,pch=18,alpha=0.6)+
          xlim(c(-1,lim))+
          ylim(c(-1,lim))+
          labs(col=NULL)+
          scale_color_manual(values = tableColor)+
          coord_equal()+
          myTheme
        
      }
      
      ###intercrop 2+ intercrop 3 #####
      if ( NbLines_I3>0 & NbLines_I2>0 & NbLines_I1==0){
        
        Id=paste0(design$density,' palms.ha-1')
        Id2=paste0(I1$density,' ',Int2,'.ha-1')
        Id3=paste0(I3$density,' ',Int3,'.ha-1')
        names(tableColor)=c(Id,Int1,Id2,Id3)
        
        
        gr=ggplot()+
          geom_point(data=I2$designPalm,aes(x=x,y=y,col= Id),shape=8,size=pointSize)+
          geom_point(data=I2$designI,aes(x=x,y=y,col=Id2),size=input$pointSizeInt2,pch=15,alpha=0.6)+
          geom_point(data=I3$designI,aes(x=x,y=y,col=Id3),size=input$pointSizeInt3,pch=18,alpha=0.6)+
          xlim(c(-1,lim))+
          ylim(c(-1,lim))+
          labs(col=NULL)+
          scale_color_manual(values = tableColor)+
          coord_equal()+
          myTheme
        
      }
      
      ###intercrop 1+ intercvrop 2+ intercrop 3 #####
      if ( NbLines_I1>0 & NbLines_I2>0 & NbLines_I3>0){
        
        Id=paste0(design$density,' palms.ha-1')
        Id1=paste0(I1$density,' ',Int1,'.ha-1')
        Id2=paste0(I1$density,' ',Int2,'.ha-1')
        Id3=paste0(I3$density,' ',Int3,'.ha-1')
        names(tableColor)=c(Id,Id1,Id2,Id3)
        
        
        gr=ggplot()+
          geom_point(data=I1$designPalm,aes(x=x,y=y,col= Id),shape=8,size=pointSize)+
          geom_point(data=I1$designI,aes(x=x,y=y,col=Id1),size=input$pointSizeInt1,alpha=0.6)+
          geom_point(data=I2$designI,aes(x=x,y=y,col=Id2),size=input$pointSizeInt2,pch=15,alpha=0.6)+
          geom_point(data=I3$designI,aes(x=x,y=y,col=Id3),size=input$pointSizeInt3,pch=18,alpha=0.6)+
          xlim(c(-1,lim))+
          ylim(c(-1,lim))+
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
    cond<- cond()
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




