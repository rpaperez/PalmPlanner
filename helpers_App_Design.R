# Load packages -----------------------------------------------------------
packs <- c("lubridate", "stringr", 'tidyverse','viridis','data.table','yaml','png','cowplot','magick')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

# inputs ------------------------------------------------------------------


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

# GENERATE DESIGN-------------------------------------------------------------------------


#' Function to visualization of the designs
#'
#' @param dist_intra distance within row of palm trees (m)
#' @param dist_inter distance between rows of palm trees (m)
#' @param dist_intercrop distance between multiple rows of palm trees (m), for intercropping 
#' @param designType type of design (square,square2,quincunx,quincunx2,quincunx3,square3,quincunx4,square4,quincunx5,square5)
#' @param lim limit of the area for plotting map (m)
#' 
#' @return
#' @export
#'
#' @examples
plot_design=function(dist_intra=NULL,dist_inter=NULL,dist_intercrop=NULL,designType=NULL,lim=50){
  

  
  if(dist_intra<0){
    print('ERROR: Intra row distance must be > 0')
    return(warng)
  }
  if(dist_inter<0){
    print('ERROR: Inter row distance must be > 0')
    return(NULL)
  }
  if(dist_intercrop<0){
    print('ERROR: Intercropping distance must be >= 0')
    return(NULL)
  }
  
  
  if (!designType %in% c('square','square2','quincunx','quincunx2','quincunx3','square3','quincunx4','square4','quincunx5','square5')){
    print('ERROR: please select a designType among square quincunx quincunx3 square3 quincunx4 square4 quincunx5 square5')
    return(NULL)
  }
  # Voronoi of the design:
  
  if (designType=='square'){
    if(dist_intercrop>0){
      print('ERROR: Intercropping distance must be 0 m for square design')
      return(NULL)
    }
    x1=dist_inter/2
    y1=dist_intra/2
    xmax=x1+dist_inter/2
    ymax=y1+dist_intra/2
    
    
    voronoi_plot=data.frame(x= x1,
                            y= y1,
                            xmin= 0,xmax=xmax,
                            ymin= 0, ymax= ymax)
    
  }
  
  
  if (designType=='quincunx'){
    if(dist_intercrop>0){
      print('ERROR: Intercropping distance must be 0m for quincunx design')
      return(NULL)
    }
    x1=dist_inter/2
    y1=dist_intra/4
    x2=dist_inter/2+dist_inter
    y2=dist_intra/4+dist_intra/2
    xmax=x2+dist_inter/2
    ymax=y2+dist_intra/4
    
    voronoi_plot=data.frame(x= c(x1,x2),
                            y= c(y1,y2),
                            xmin= 0,xmax=xmax,
                            ymin= 0, ymax= ymax)
  }
  
  
  if (designType=='quincunx2'){
    
    if(dist_intercrop==0){
      print('ERROR: Intercropping distance must > 0 m for quincunx2 design')
      return(NULL)
    }

    x1=dist_intercrop/2
    y1=dist_intra/4
    x2=x1+dist_inter
    y2=y1+dist_intra/2
    x3=x2+dist_intercrop
    y3=y2
    x4=x3+dist_inter
    y4=y1
      
    xmax=2*dist_intercrop+2*dist_inter
    ymax=dist_intra
    
    voronoi_plot= data.frame(x= c(x1,x2,x3,x4),
                             y= c(y1,y2,y3,y4),
                             xmin= 0,xmax=xmax,
                             ymin= 0, ymax= ymax)
    
  }
  
  if (designType=='square2'){
    
    if(dist_intercrop==0){
      print('ERROR: Intercropping distance must > 0 m for square2 design')
      return(NULL)
    }
    x1=dist_inter/2
    y1=dist_intra/2
    
    x2=x1+dist_intercrop
    y2=y1
    
    xmax=x2+dist_inter/2
    ymax=y2+dist_intra/2
    
    voronoi_plot= data.frame(x= c(x1,x2),
                             y= c(y1,y2),
                             xmin= 0,xmax=xmax,
                             ymin= 0, ymax= ymax)
    
  }
  
  
  if (designType=='quincunx3'){
    if(dist_intercrop==0){
      print('ERROR:please provide dist_intercrop in quincunx3 design')
      return(NULL)}
    
    x1=dist_inter/2
    y1=dist_intra/4
    x2=x1+dist_inter
    y2=y1+dist_intra/2
    x3=x2+dist_intercrop
    y3=y2
    xmax=x3+dist_inter/2
    ymax=y3+dist_intra/4
    
    voronoi_plot= data.frame(x= c(x1,x2,x3),
                             y= c(y1,y2,y3),
                             xmin= 0, xmax= xmax,
                             ymin= 0, ymax= ymax)
    
  }
  
  if (designType=='square3'){
    if(dist_intercrop==0){
      print('ERROR:please provide dist_intercrop in square3 design')
      return(NULL)}
    
    x1=dist_inter/2
    y1=dist_intra/2
    x2=x1+dist_inter
    y2=y1
    x3=x2+dist_intercrop
    y3=y2
    xmax=x3+dist_inter/2
    ymax=y3+dist_intra/2
    
    voronoi_plot= data.frame(x= c(x1,x2,x3),
                             y= c(y1,y2,y3),
                             xmin= 0, xmax= xmax,
                             ymin= 0, ymax= ymax)
    
  }
  
  if (designType=='quincunx4'){
    if(dist_intercrop==0){
      print('please provide dist_intercrop in quincunx4 design')
      return(NULL)}
    
    x1=dist_inter/2
    y1=dist_intra/4
    x2=x1+dist_inter
    y2=y1+dist_intra/2
    x3=x2+dist_intercrop
    y3=y2
    x4=x3+dist_inter
    y4=y1
    x5=x4+dist_inter
    y5=y2
    x6=x5+dist_inter
    y6=y1
    x7=x6+dist_intercrop
    y7=y1
    x8=x7+dist_inter
    y8=y2
    
    xmax=x8+dist_inter/2
    ymax=y8+dist_intra/4

    voronoi_plot= data.frame(x= c(x1,x2,x3,x4,x5,x6,x7,x8),
                             y= c(y1,y2,y3,y4,y5,y6,y7,y8),
                             xmin= 0, xmax= xmax,
                             ymin= 0, ymax= ymax)
    
  }
  
  if (designType=='square4'){
    if(dist_intercrop==0){
      print('please provide dist_intercrop in square4 design')
      return(NULL)}
    
    x1=dist_inter/2
    y1=dist_intra/2
    x2=x1+dist_inter
    y2=y1
    x3=x2+dist_intercrop
    y3=y1
    x4=x3+dist_inter
    y4=y2
    xmax=x4+dist_inter/2
    ymax=y4+dist_intra/2
    
    voronoi_plot= data.frame(x= c(x1,x2,x3,x4),
                             y= c(y1,y2,y3,y4),
                             xmin= 0, xmax= xmax,
                             ymin= 0, ymax= ymax)
    
  }
  
  if (designType=='quincunx5'){
    if(dist_intercrop==0){
      print('please provide dist_intercrop in quincunx5 design')
      return(NULL)}
    
    
    x1=dist_inter/2
    y1=dist_intra/4
    x2=x1+dist_inter
    y2=y1+dist_intra/2
    x3=x2+dist_intercrop
    y3=y2
    x4=x3+dist_inter
    y4=y1
    x5=x4+dist_inter
    y5=y3
    xmax=x5+dist_inter/2
    ymax=y5+dist_intra/4
    
    voronoi_plot= data.frame(x= c(x1,x2,x3,x4,x5),
                             y= c(y1,y2,y3,y4,y5),
                             xmin= 0, xmax= xmax,
                             ymin= 0, ymax= ymax)
    
  }
  
  if (designType=='square5'){
    if(dist_intercrop==0){
      print('please provide dist_intercrop in square5 design')
      return(NULL)}
    
    
    x1=dist_inter/2
    y1=dist_intra/2
    x2=x1+dist_inter
    y2=y1
    x3=x2+dist_intercrop
    y3=y2
    x4=x3+dist_inter
    y4=y1
    x5=x4+dist_inter
    y5=y3
    xmax=x5+dist_inter/2
    ymax=y5+dist_intra/2
    
    voronoi_plot= data.frame(x= c(x1,x2,x3,x4,x5),
                             y= c(y1,y2,y3,y4,y5),
                             xmin= 0, xmax= xmax,
                             ymin= 0, ymax= ymax)
    
  }
  
  # number of rows and columns in 1 ha
  repRows=ceiling(lim/(dist_inter+dist_intercrop))
  repCol=ceiling(lim/dist_intra)
  
  # Matrix of the design (each cell is a Voronoi):
  mat_plot= expand.grid(repRows= 1:repRows, repCol= 1:repCol)
  
  
  # Full design:
  design=
    mapply(function(repRows,repCol){
      voronoi_plot%>%
        dplyr::select(x,y,xmax,ymax,xmin,ymin)%>%
        dplyr::mutate(xmin= xmax*(repRows-1), ymin= ymax*(repCol-1),
                      x= x+xmin, y= y+ymin,
                      xmax= xmax*repCol, ymax= ymax*repRows,
                      repCol= repCol, repRows= repRows)
    }, repRows= mat_plot$repRows, repCol= mat_plot$repCol)%>%t()%>%
    dplyr::as_tibble()%>%
    tidyr::unnest(cols = c(x, y, xmax, ymax, xmin, ymin, repCol, repRows))%>%
    dplyr::mutate(xmax= max(xmax), ymax= max(ymax),
                  xmin= min(xmin), ymin= min(ymin))
  
  # output
    list(design=design)
}


# add intercrop design ----------------------------------------------------

sizeDesign=data.frame(designType=c('square','quincunx','square2','quincunx2','square3','quincunx3','square4','quincunx4','square5','quincunx5'),
                      nLines=c(1,1,2,2,3,3,4,4,5,5),
                      firstLines=c(1,1,2,4,3,3,4,8,5,5)) %>% 
  mutate(interLines=nLines-1)

#' design_intercrop generate an intercrop design
#'
#' @param dist_intra distance within row of palm trees (m)
#' @param dist_inter distance between rows of palm trees (m)
#' @param dist_intercrop distance between multiple rows of palm trees (m), for intercropping 
#' @param designType type of design for oil palm (square,square2,quincunx,quincunx2,quincunx3,quincunx4,quincunx5)
#' @param lim limit of the area for plotting map (m)
#' @param I_dist_intra distance within row of intercrop (m)
#' @param I_dist_inter distance between rows of intercrop (m)
#' @param I_designType type of design for intercrop (square,square2,quincunx,quincunx2,quincunx3,quincunx4,quincunx5)
#'
#' @return Design ((x,y) coordinates of plants)
#' @export
#'
#' @examples
design_intercrop=function(dist_intra=NULL,dist_inter=NULL,dist_intercrop=NULL,designType=NULL,lim=50,I_dist_intra=NULL,I_dist_inter=NULL,I_designType=NULL){
  
  ### get distance between intercrop edge and palms row
  dist_edge=(dist_intercrop-sizeDesign[sizeDesign==I_designType,]$interLines*I_dist_inter)/2

  if(designType %in% c('square','quincunx')){
    dist_intercrop=0
    dist_edge=(dist_inter-sizeDesign[sizeDesign==I_designType,]$interLines*I_dist_inter)/2
    if(I_designType %in% c('square','quincunx')){
      dist_edge=dist_inter/2
    }
  }

  if((dist_edge)<0){
    print('!!! intercroping area is larger than intercropping space between palm rows')
    return(NULL)
  }
  
  ### first generate palm design
  a=plot_design(dist_intra=dist_intra,dist_inter=dist_inter,dist_intercrop=dist_intercrop,designType=designType,lim=lim)
  
  #### then generate intercrops design
  
  ### get distance between 2 consecutive intercropping areas (D_intercrops)
  D_intercrops=2*dist_edge+sizeDesign[sizeDesign==designType,]$interLines*dist_inter
  

  if(I_designType %in% c('square','quincunx')){
    I_dist_inter=D_intercrops
    D_intercrops=0
    print(paste('I_dist_inter is fixed to: ',I_dist_inter,'m'))
  }
  

  b=plot_design(dist_intra=I_dist_intra,dist_inter=I_dist_inter,dist_intercrop=D_intercrops,designType=I_designType,lim=2*lim) 
  
  ### keep full intercropping area (remove first rows)
  I_removeL=min(2,sizeDesign[sizeDesign$designType==I_designType,]$firstLines-1) ### lines to remove

  I_start=ifelse(I_removeL>0,unique(b$design$x)[I_removeL],0)
  
  offSetL=max(1,min(2,sizeDesign[sizeDesign$designType==designType,]$firstLines-1)) ### first lines  in palm design to get the offset of intercropping area
  
  interCrop=  b$design %>% 
    filter(x>I_start) %>% 
    mutate(x=x-min(x)+unique(a$design$x)[offSetL]+dist_edge,y=y-min(y)) 

  # outputs
  result=list(designPalm=a$design,density=b$density,designI=interCrop)
  return(result)
}

