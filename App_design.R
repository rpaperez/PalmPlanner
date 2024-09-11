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
                            colourpicker::colourInput("colPalm", h4("Select colour"), "forestgreen")
                            
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
                            colourpicker::colourInput("colInt1", h4("Select colour"), "orange"),
                            
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
                            colourpicker::colourInput("colInt2", h4("Select colour"), "red"),
                            
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
                            colourpicker::colourInput("colInt3", h4("Select colour"), "purple"),
                            
                   ),
                   tabPanel("Crop in palm row",
                            textInput(inputId = "Int4",label =  h4("Enter the name of the crop"),value = 'Intercrop 4'),
                            numericInput("intra_distC1",label = h4("Intra row distance (m):"), value=NULL,step = 0.1,min = 0),
                            numericInput("offset_Y_C1",label = h4("y offset (m)"), value=0,step = 0.05,max = 0),
                            
                            tags$hr(),
                            numericInput("pointSizeC1",label = h4("Select point size:"), value=2,step =0.2,min = 0),
                            colourpicker::colourInput("colC1", h4("Select colour"), "blue")
                            
                   )
                   
                   ,
                   
                   tabPanel("replace Palm",
                            textInput(inputId = "Replace",label =  h4("Enter the name of the crop"),value = 'Intercrop 5'),
                            tags$hr(),
                            numericInput("pointSizeReplace",label = h4("Select point size:"), value=2,step =0.2,min = 0),
                            colourpicker::colourInput("colReplace", h4("Select colour"), "brown"),
                            actionButton('replace', 'Replace deleted palms')
                            
                   )
                   
                   ,
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
                            colourpicker::colourInput("colR", h4("Select colour"), "grey"),
                            
                   )
                 ),
                 tags$hr(),
                 tags$hr(),
                 numericInput("lim",label = h4("plot limits (m):"), value=100,step = 10,min = 0),
                 checkboxInput('origin',label = 'Set origin to the first palm',value = F)
               ),
               mainPanel(
                 tabsetPanel(
                   
                   tabPanel('plot',
                            actionButton('delete', 'Delete selected points'), 
                            plotlyOutput('plot')%>% withSpinner(color="darkorange")
                            
                   ),
                   tabPanel('Selection',
                            actionButton('reset', 'Reset data'),
                            # checkboxInput('selectReset',label = 'Select the table to reset',value = T),
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
  
  pointSize<- reactive({
    input$pointSize
  })
  
  lim<- reactive({
    input$lim
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
  
  ##inner crop 1 parameters
  inter_distC1<- reactive({
    input$inter_distC1
  })
  
  intra_distC1<- reactive({
    input$intra_distC1
  })
  
  dist_intercropC1<- reactive({
    input$dist_intercropC1
  })
  
  
  designTypeC1<- reactive({
    input$designTypeC1
  })
  
  NbRemovedC1<- reactive({
    input$NbRemovedC1
  })
  
  pointSizeC1<- reactive({
    input$pointSizeC1
  })
  
  offset_Y_C1<- reactive({
    input$offset_Y_C1
  })
  
  Int4<- reactive({
    input$Int4
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
  selection=reactiveVal() 
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
      
      if(  input$NbLines_I1==1 & !is.na(input$inter_dist_I1) | input$NbLines_I1==1 & is.na(input$inter_dist_I1)){
        
        updateNumericInput(session, "inter_dist_I1", value = 0)
      }
      if(  input$NbLines_I1==1 & is.na(input$intra_dist_I1) | input$NbLines_I1==1 & input$intra_dist_I1==0){   updateNumericInput(session, "intra_dist_I1", value = 5)
      }
      
      if(input$NbLines_I2==1 &  !is.na(input$inter_dist_I2) | input$NbLines_I2==1 & is.na(input$inter_dist_I2)){
        updateNumericInput(session, "inter_dist_I2", value = 0)
      }
      if(  input$NbLines_I2==1 & is.na(input$intra_dist_I2)|input$NbLines_I2==1 & input$intra_dist_I2==0){
        updateNumericInput(session, "intra_dist_I2", value = 6)
      }
      
      if(input$NbLines_I3==1 &  !is.na(input$inter_dist_I3) | input$NbLines_I3==1 & is.na(input$inter_dist_I3)){
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
      
      dist_inter=inter_dist()
      dist_intra=intra_dist()
      dist_intercrop=dist_intercrop()
      designType=designType()
      NbRemoved=NbRemoved()
      pointSize=pointSize()
      
      origin=origin()
      lim=lim()
      orientation='NS'
      
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
      
      
      intra_distC1=intra_distC1()
      designTypeC1=designTypeC1()
      offset_Y_C1=offset_Y_C1()
      NbRemovedC1=NbRemovedC1()
      pointSizeC1=pointSizeC1()
      Int4=Int4()
      
      if (is.null(dist_inter) | is.null(dist_intra)) {
        return(NULL)
      }
      
      ###init
      
      I1=NULL
      I2=NULL
      I3=NULL
      C1=NULL
      replanting=NULL
      x_offset=0
      y_offset=0
      
      #### map
      designRef=tableDesign %>% filter(design==designType & NbR==NbRemoved) %>% select(designRef)
      # print(paste('designRef=',designRef))
      
      design=plot_design(dist_intra=dist_intra,dist_inter=dist_inter,dist_intercrop=dist_intercrop,designType=designRef[[1]],orientation=orientation,pointSize=pointSize,lim=lim)
      
      if(origin==T){
        x_offset=design$design$x[1]
        y_offset=design$design$y[1]
        design$design$x=design$design$x-x_offset
        design$design$y=design$design$y-y_offset
      }
      
      design$design=design$design %>% 
        mutate(type='palms',
               name='palms',
               Id=paste0(design$density,' palms.ha-1'),
               pointS=input$pointSize)
      
      
      ### with intercrops####
      if ( NbLines_I1>0){
        
        designRef_I1=tableDesign %>% filter(design==designType_I1 & NbLines==NbLines_I1) %>% select(designRef)
        
        I1=design_intercrop(dist_intra =dist_intra,dist_inter =dist_inter,designType =designRef[[1]],dist_intercrop =dist_intercrop,orientation = orientation,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I1,I_dist_inter =inter_dist_I1,I_designType = designRef_I1[[1]])
        
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
                 name=Int1,
                 Id=paste0(I1$density,' ',Int1,'.ha-1'),
                 pointS=input$pointSizeInt1)
      }
      
      if ( NbLines_I2>0){
        designRef_I2=tableDesign %>% filter(design==designType_I2 & NbLines==NbLines_I2) %>% select(designRef)
        
        I2=design_intercrop(dist_intra =dist_intra,dist_inter =dist_inter,designType =designRef[[1]],dist_intercrop =dist_intercrop,orientation = orientation,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I2,I_dist_inter =inter_dist_I2,I_designType = designRef_I2[[1]])
        
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
                 name=Int2,
                 Id=paste0(I2$density,' ',Int2,'.ha-1'),
                 pointS=input$pointSizeInt2)
      }
      
      
      if ( NbLines_I3>0){
        designRef_I3=tableDesign %>% filter(design==designType_I3 & NbLines==NbLines_I3) %>% select(designRef)
        
        
        I3=design_intercrop(dist_intra =dist_intra,dist_inter =dist_inter,designType =designRef[[1]],dist_intercrop =dist_intercrop,orientation = orientation,pointSize =pointSize,lim = lim, I_dist_intra = intra_dist_I3,I_dist_inter =inter_dist_I3,I_designType = designRef_I3[[1]])
        
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
                 name=Int3,
                 Id=paste0(I3$density,' ',Int3,'.ha-1'),
                 pointS=input$pointSizeInt3)
        
      }
      
      ### CropInLine 1
      if (!is.na(intra_distC1)){
        
        C1=plot_design(dist_intra=intra_distC1,dist_inter=dist_inter,dist_intercrop=dist_intercrop,designType=designRef[[1]],orientation=orientation,pointSize=pointSizeC1,lim=2*lim)
        
        if(origin==T){
          C1$design$x=C1$design$x-x_offset
          C1$design$y=C1$design$y-y_offset
        }
        
        C1$design$y= C1$design$y+offset_Y_C1
        
        C1$design=C1$design %>%
          mutate(type='C1',
                 name=Int4,
                 Id=paste0(C1$density,' ',Int4,'.ha-1'),
                 pointS=input$pointSizeC1)
        
      }
      
      #### add old planting#####
      if (replant==T){
        
        designRefRep=tableDesign %>% filter(design==designTypeR & NbR==0) %>% select(designRef)
        
        
        replanting=plot_design(dist_intra=intra_distR,dist_inter=inter_distR,dist_intercrop=0,designType=designRefRep[[1]],orientation=orientation,pointSize=pointSize,lim=2*lim)
        
        if(origin==T){
          replanting$design$x=replanting$design$x-x_offset
          replanting$design$y=replanting$design$y-y_offset
        }
        
        replanting$design$x= replanting$design$x+offset_X_R
        replanting$design$y= replanting$design$y+offset_Y_R
        
        replanting$design=replanting$design %>% 
          mutate(type='old palms',
                 name='old palms',
                 Id=paste0(replanting$density,' old palms.ha-1'),
                 pointS=input$pointSize)
      }
      don=bind_rows(design$design,I1$designI,I2$designI,I3$designI,replanting$design,C1$design) %>% 
        mutate(code=paste(x,y,name))
      
      result=list(don=don,offsets=data.frame(x_offset=x_offset,y_offset=y_offset))
      # print(head(don))
      return(result)
      
    })
  })
  
  
  
  # remove points -----------------------------------------------------------
  
  observeEvent(input$action, {
    df(result()$don)
  })
  
  selectP <- reactive({
    d <- event_data("plotly_click",source='A')
    l <- event_data("plotly_selected",source='A')
  
    selectP=rbind(d,l) 
    return(selectP)
  })
  
  values <- reactiveValues()
  values$df <- data.frame(x = NA, y = NA,name=NA)
  
  newEntry <- reactive({
    

    repl_sub=df() %>% 
      mutate(xy=paste(x,y)) %>% 
      filter((xy %in% paste(selectP()$x,selectP()$y))) %>% 
      select(-xy)
    

    
    newLine <- data.frame(x=repl_sub$x, y=repl_sub$y,name=repl_sub$name)
    
    isolate({newEntry=values$df <- rbind(values$df,newLine)  
    
    newEntry=newEntry%>% 
      filter(name %in% c('old palms','palms',input$Int1,input$Int2,input$Int3,input$Int4)) %>% 
      mutate(xy=paste(x,y),
             code=paste(x,y,name),
             codeP=paste(x,y,'palms')) %>% 
      distinct()
    ### remove selected points of intercrops overlapping palm points
    newEntry=newEntry%>% 
      mutate(overlap=ifelse(codeP %in% unique(newEntry$code) & !(name %in% c('palms')),T,F)) %>% 
      filter(overlap==F)})
    return(newEntry)
  })
  
  observeEvent(input$delete,{
    # sub=result()$don %>% filter(!(code %in% paste(newEntry()$x,newEntry()$y,newEntry()$name)))
    sub=result()$don %>% filter(!(code %in% paste(selection()$x,selection()$y,selection()$name)))
    df(sub)
    
  })
  
  # reset selection --------------------------------------------------------
  
  observeEvent(input$reset,{
    values$df <- data.frame(x = NA, y = NA,name=NA)
    selection(NULL)
    df(result()$don)
  })  
  
  # replace palms points -----------------------------------------------------------
  
  observeEvent(input$replace,{
    replaceP=newEntry() %>%
      filter(name=='palms') 
    
    subPrev=df() %>% 
      mutate(xy=paste(x,y)) %>% 
      filter(!(xy %in% unique(replaceP$xy))) %>% 
      select(type,x,y,Id,xmax,ymax,repCol,repRows,name)
    
    subRepl=result()$don %>% 
      mutate(xy=paste(x,y)) %>% 
      filter(xy %in% unique(replaceP$xy)) %>% 
      mutate(type='replacement',
             name=input$Replace,
             Id=input$Replace,
             pointS=input$pointSizeR) %>% 
      select(type,x,y,Id,xmax,ymax,repCol,repRows,name)
    
    df(rbind(subPrev,subRepl))
    
  })
  
  
  
  #### graphic####
  plot<-reactive({
    
    cond<- cond()
    lim=lim()
    # result=result()
    
    df<- df()
    if (is.null(df)){
      # df(result()$don)
      return(NULL)
    } 
    
    # print(summary(df()))
    selection(newEntry() %>% select(x,y,name) )
    
    tableColor=c(input$colPalm,input$colInt1,input$colInt2,input$colInt3,input$colC1,input$colReplace,input$colR)
    
    ## calculate density after removing plants (removing also offsets for a propoer estimation of density)
    x_offset=result()$offsets$x_offset
    y_offset=result()$offsets$y_offset
    
    
    # get voronoi limits (in m)
    xmax_unit=(max(df$xmax))/(max(df$repCol))
    ymax_unit=(max(df$ymax))/(max(df$repRows))
    
    # number of voronoi replicated in lim
    repX=(lim+x_offset)%/%xmax_unit
    repY=(lim+y_offset)%/%ymax_unit
    
    limX=repX*xmax_unit
    limY=repY*ymax_unit
    
    ### removing offsets
    
    df_sub=df %>% mutate(x=x+x_offset,y=y+y_offset) 
    df_sub[df_sub$type=='old palms',]$x=df_sub[df_sub$type=='old palms',]$x-offset_X_R()
    df_sub[df_sub$type=='old palms',]$y=df_sub[df_sub$type=='old palms',]$y-offset_Y_R()
    df_sub[df_sub$type=='I1',]$y=df_sub[df_sub$type=='I1',]$y+offset_Y_I1()
    df_sub[df_sub$type=='I2',]$y=df_sub[df_sub$type=='I2',]$y+offset_Y_I2()
    df_sub[df_sub$type=='I3',]$y=df_sub[df_sub$type=='I3',]$y+offset_Y_I3()
    df_sub[df_sub$type=='C1',]$y=df_sub[df_sub$type=='C1',]$y+offset_Y_C1()
    
    
    df_sub=df_sub%>%  filter(x<=limX & y<=limY)
    
    
    density_palms=paste(floor(nrow(df_sub[df_sub$type=='palms',])/((limX/100)*(limY/100))),'palms.ha-1')
    density_I1=paste(floor(nrow(df_sub[df_sub$type=='I1',])/((limX/100)*(limY/100))),input$Int1,'.ha-1')
    density_I2=paste(floor(nrow(df_sub[df_sub$type=='I2',])/((limX/100)*(limY/100))),input$Int2,'.ha-1')
    density_I3=paste(floor(nrow(df_sub[df_sub$type=='I3',])/((limX/100)*(limY/100))),input$Int3,'.ha-1')
    density_C1=paste(floor(nrow(df_sub[df_sub$type=='C1',])/((limX/100)*(limY/100))),input$Int4,'.ha-1')
    density_replacement=paste(floor(nrow(df_sub[df_sub$type=='replacement',])/((limX/100)*(limY/100))),input$Replace,'.ha-1')
    density_old=paste(floor(nrow(df_sub[df_sub$type=='old palms',])/((limX/100)*(limY/100))),'old palms.ha-1')

    df[df$type=='palms','Id']=density_palms
    df[df$type=='I1','Id']=density_I1
    df[df$type=='I2','Id']=density_I2
    df[df$type=='I3','Id']=density_I3
    df[df$type=='C1','Id']=density_C1
    df[df$type=='replacement','Id']=density_replacement
    df[df$type=='old palms','Id']=density_old
    
    
    names(tableColor)=c(unique(df[df$type=='palms','Id']),
                        unique(df[df$type=='I1','Id']),
                        unique(df[df$type=='I2','Id']),
                        unique(df[df$type=='I3','Id']),
                        unique(df[df$type=='C1','Id']),
                        unique(df[df$type=='replacement','Id']),
                        unique(df[df$type=='old palms','Id']))
    
    
    
    tableShape=c(8,1,15,18,16,5,4)
    names(tableShape)=names(tableColor)
    
    tableSize=c(input$pointSize,input$pointSizeInt1,input$pointSizeInt2,input$pointSizeInt3,input$pointSizeC1,input$pointSizeReplace,input$pointSizeR)
    
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
    
    gr=ggplotly(gr,height = 800,width=800,source='A') %>% layout(dragmode = "lasso")
    return(gr)
    
  })
  
  # Outputs -----------------------------------------------------------------
  
  ### plot
  output$plot=renderPlotly({plot()})
  
  output$ui.action <- renderUI({
    actionButton("action", "Visualize the design")
  })
  
  output$table<- renderTable({
    selection()
  })
  
}

shinyApp(ui, server)




