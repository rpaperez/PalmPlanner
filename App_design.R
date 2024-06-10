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
                 uiOutput('ui.action'),
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
                            
                            numericInput("inter_dist",label = h4("Inter palm row distance (m):"), value=7.8,step = 0.1,min = 0),
                            numericInput("intra_dist",label = h4("Intra palm row distance (m):"), value=9,step = 0.1,min = 0),
                            numericInput("dist_intercrop",label = h4("Intercropping distance (m):"), value=0,step = 0.1,min = 0),
                            
                            tags$hr(),
                            numericInput("pointSize",label = h4("Select point size:"), value=10,step =0.2,min = 0),
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
                            numericInput("pointSizeInt1",label = h4("Select point size:"), value=2,step = 0.2,min = 0),
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
                            numericInput("offset_Y_I2",label = h4("y offset (m)"), value=0,step = 0.05,max = 0),
                            tags$hr(),
                            numericInput("pointSizeInt2",label = h4("Select point size:"), value=2,step = 0.2,min = 0),
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
                            numericInput("offset_Y_I3",label = h4("y offset (m)"), value=0,step = 0.05,max = 0),
                            tags$hr(),
                            numericInput("pointSizeInt3",label = h4("Select point size:"), value=2,step = 0.2,min = 0),
                            colourInput("colInt3", h4("Select colour"), "purple"),
                            
                   ),
                   tabPanel("Replanting",
                            checkboxInput('replant',label = 'visualize old palms',value = F),
                            selectInput(inputId="designTypeR", label = h4("Select design pattern"),
                                        choices = list("square" = 'square',
                                                       'quincunx'='quincunx'),
                                        selected = 'quincunx'),
                            numericInput("inter_distR",label = h4("Inter palm row distance (m):"), value=7.8,step = 0.1,min = 0),
                            numericInput("intra_distR",label = h4("Intra palm row distance (m):"), value=9,step = 0.1,min = 0),
                            numericInput("offset_Y_R",label = h4("y offset (m)"), value=-4.5,step = 0.05,max = 0),
                            numericInput("offset_X_R",label = h4("x offset (m)"), value=-3.9,step = 0.05,max = 0),
                            numericInput("pointSizeR",label = h4("Select point size:"), value=5,step = 0.2,min = 0),
                            
                            
                            
                   )
                   
                 ),
                 tags$hr(),
                 tags$hr(),
                 numericInput("lim",label = h4("plot limits (m):"), value=50,step = 10,min = 0),
                 checkboxInput('origin',label = 'Set origin to the first palm',value = T)
               ),
               mainPanel(
                 tabsetPanel(
                   
                   tabPanel('plot',
                            actionButton('delete', 'Delete selected points'), 
                            actionButton('ResetData', 'Reset data'),
                            plotlyOutput('plot')%>% withSpinner(color="darkorange")
                   
                   ),
                   tabPanel('Selection',
                            tableOutput('table')
                   )
                 )
               )
               
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
  
  ####replanting
  
  replant<- reactive({
    input$replant
  })
  
  designTypeR<- reactive({
    input$designTypeR
  })
  
  inter_distR<- reactive({
    input$inter_distR
  })
  
  intra_distR<- reactive({
    input$intra_distR
  })
  
  offset_Y_R<- reactive({
    input$offset_Y_R
  })
  
  offset_X_R<- reactive({
    input$offset_X_R
  })
  

  
  sub=NULL
  df=reactiveVal()
  
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
      if(  input$NbLines_I1==1 & is.na(input$intra_dist_I1) | input$NbLines_I1==1 & input$intra_dist_I1==0){   updateNumericInput(session, "intra_dist_I1", value = 5)
      }
      
      if(input$NbLines_I2==1 &  !is.na(input$inter_dist_I2)){
        updateNumericInput(session, "inter_dist_I2", value = 0)
      }
      if(  input$NbLines_I2==1 & is.na(input$intra_dist_I2)|input$NbLines_I2==1 & input$intra_dist_I2==0){
        updateNumericInput(session, "intra_dist_I2", value = 6)
      }
      
      if(input$NbLines_I3==1 &  !is.na(input$inter_dist_I3)){
        updateNumericInput(session, "inter_dist_I3", value = 0)
      }
      if(  input$NbLines_I3==1 & is.na(input$intra_dist_I3)|input$NbLines_I3==1 & input$intra_dist_I3==0){
        updateNumericInput(session, "intra_dist_I3", value = 7)
      }
      
      ## avoid inter row null when multiple row design is selected
      
      if(  input$NbLines_I1>1 & is.na(input$inter_dist_I1) | input$NbLines_I1>1 & !is.na(input$inter_dist_I1) & input$inter_dist_I1==0){
        updateNumericInput(session, "inter_dist_I1", value = 3)
      }
      if(  input$NbLines_I1>1 & is.na(input$intra_dist_I1) | input$NbLines_I1>1 & !is.na(input$intra_dist_I1) & input$intra_dist_I1==0){
        updateNumericInput(session, "intra_dist_I1", value = 5)
      }
      
      if(  input$NbLines_I2>1 &  is.na(input$inter_dist_I2)| input$NbLines_I2>1 & input$inter_dist_I2==0){
        updateNumericInput(session, "inter_dist_I2", value = 2)
      }
      if(  input$NbLines_I2>1 & is.na(input$intra_dist_I2) | input$NbLines_I2>1 & input$intra_dist_I2==0){      updateNumericInput(session, "intra_dist_I2", value = 7)
      }
      
      if(  input$NbLines_I3>1 &  is.na(input$inter_dist_I3)| input$NbLines_I3>1 & input$inter_dist_I3==0){
        updateNumericInput(session, "inter_dist_I3", value = 1)
      }
      if(  input$NbLines_I3>1 & is.na(input$intra_dist_I3) | input$NbLines_I3>1 & input$intra_dist_I3==0){      updateNumericInput(session, "intra_dist_I3", value = 9)
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
      
      designTypeR=designTypeR()
      inter_distR=inter_distR()
      intra_distR=intra_distR()
      offset_Y_R=offset_Y_R()
      offset_X_R=offset_X_R()
      replant=replant()

      
      if (is.null(d_inter) | is.null(d_intra)) {
        return(NULL)
      }
      
      ###init
      
      I1=NULL
      I2=NULL
      I3=NULL
      replanting=NULL
      
      #### map
      designRef=tableDesign %>% filter(design==designType & NbR==NbRemoved) %>% select(designRef)
      
      design=plot_design(dist_intra=d_intra,dist_inter=d_inter,dist_intercrop=dist_intercrop,designType=designRef[[1]],orientation=orientation,pointSize=pointSize,lim=lim,twist=0)
      
      if(origin==T){
        x_offset=design$design$x[1]
        y_offset=design$design$y[1]
        design$design$x=design$design$x-x_offset
        design$design$y=design$design$y-y_offset
      }
      
      design$design=design$design %>% 
        mutate(type='palms',
               Id=paste0(design$density,' palms.ha-1'),
               pointS=input$pointSize)
      
      
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
        I1$designI=I1$designI %>% 
          mutate(type='I1',
                 Id=paste0(I1$density,' ',Int1,'.ha-1'),
                 pointS=input$pointSizeInt1)
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
        I2$designI= I2$designI%>% 
          mutate(type='I2',
                 Id=paste0(I2$density,' ',Int2,'.ha-1'),
                 pointS=input$pointSizeInt2)
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
        
        I3$designI= I3$designI %>% 
          mutate(type='I3',
                 Id=paste0(I3$density,' ',Int3,'.ha-1'),
                 pointS=input$pointSizeInt3)
        
      }
      
      #### add old planting#####
      if (replant==T){
        
        designRefRep=tableDesign %>% filter(design==designTypeR & NbR==0) %>% select(designRef)
        
        
        replanting=plot_design(dist_intra=intra_distR,dist_inter=inter_distR,dist_intercrop=0,designType=designRefRep[[1]],orientation=orientation,pointSize=pointSize,lim=2*lim,twist=0)
        
        if(origin==T){
          replanting$design$x=replanting$design$x-x_offset
          replanting$design$y=replanting$design$y-y_offset
        }
        
        replanting$design$x= replanting$design$x+offset_X_R
        replanting$design$y= replanting$design$y+offset_Y_R
        
        replanting$design=replanting$design %>% 
          mutate(type='old palms',
                 Id=paste0(replanting$density,' old palms.ha-1'),
                 pointS=input$pointSize)
      }
        don=bind_rows(design$design,I1$designI,I2$designI,I3$designI,replanting$design)
        
        
      return(don)
      
    })
  })
  
  
  observeEvent(input$delete,{
    sub=df() %>% filter(!(x %in% selectP()$x & y %in% selectP()$y))
    df(sub)
    
  })
  
  observeEvent(input$ResetData, {
    df(result())
  })
  
  observeEvent(input$action, {
    df(result())
  })
  
  selectP <- reactive({
    d <- event_data("plotly_click")
    l <- event_data("plotly_selected")
    
    rbind(d,l) 
  })
  
  #### graphic####
  plot<-reactive({

    cond<- cond()
    lim=lim()
    result=result()
    
    df<- df()
    if (is.null(df)){
      df(result())
    } 
    
    
    tableColor=c(input$colPalm,input$colInt1,input$colInt2,input$colInt3,'grey')
    
    names(tableColor)=c(unique(df[df$type=='palms','Id']),
                        unique(df[df$type=='I1','Id']),
                        unique(df[df$type=='I2','Id']),
                        unique(df[df$type=='I3','Id']),
                        unique(df[df$type=='old palms','Id']))
    
    tableShape=c(8,1,15,18,4) 
    names(tableShape)=names(tableColor)
    
    tableSize=c(input$pointSize,input$pointSizeInt1,input$pointSizeInt2,input$pointSizeInt3,input$pointSizeR)
    
    names(tableSize)=names(tableColor)

      gr=ggplot(data=df,aes(x=x,y=y,col= Id,pch=Id,size=Id))+
      geom_point()+
      xlim(c(-1,lim))+
      ylim(c(-1,lim))+
      labs(col=NULL)+
      scale_color_manual(name='',values = tableColor)+
      scale_shape_manual(name='',values=tableShape)+
      scale_size_manual(name='',values=tableSize)+
        
      coord_equal()+
      myTheme
    
    gr=ggplotly(gr,height = 800,width=800,source = "A")
    return(gr)
    
  })
  
  # Outputs -----------------------------------------------------------------
  
  ### plot
  output$plot=renderPlotly({plot()})
  
  
  output$ui.action <- renderUI({
    actionButton("action", "Visualize the design")
  })
  
  output$table<- renderTable({
    selectP()[,c('x','y')]
  })
  
}

shinyApp(ui, server)




