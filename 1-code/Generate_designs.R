###############
# Script to generate and visualize the designs ----------------------------
################
# R.PEREZ February 2024


# Load packages -----------------------------------------------------------
packs <- c("lubridate", "stringr", 'tidyverse','viridis','data.table','yaml','png','cowplot','magick')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)


source('1-code/helpers.R')

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

# function ----------------------------------------------------------------

# nbRow=10
# nbCol=10

#' Function to generate OPS  and visualization of the designs
#'
#' @param dist_intra distance within row of palm trees (m)
#' @param dist_inter distance between rows of palm trees (m)
#' @param dist_intercrop distance between multiple rows of palm trees (m), for intercropping 
#' @param designType type of design (square,square2,quincunx,quincunx2,quincunx3,quincunx4,quincunx5)
#' @param orientation orientation of the scene ('NS': North-South or 'EW': East-West)
#' @param pointSize size of point in the plot
#' @param replanting allow to visualize old 9x9 quinconx positions on plots 
#' @param lim limit of the area for plotting map (m)
#' @param twist twist/rotation of the palm stem in the ops file (in degree)
#' 
#' @return
#' @export
#'
#' @examples
generate_design=function(dist_intra=NULL,dist_inter=NULL,dist_intercrop=NULL,designType=NULL,orientation=orientation,twist=twist,pointSize=5,replanting=T,lim=50){

  # l=9.21
  # h=sqrt(3*l**2/4)
  # dist_intra=l
  # dist_intercrop=2*h
  # dist_inter=h
  # designType='square'
  # pointSize=3

  if (!designType %in% c('square','square_bis','quincunx_bis','square2','quincunx','quincunx2','quincunx3','quincunx4','quincunx5')){
    print('please select a designType among square quincunx quincunx2  quincunx3 quincunx4 quincunx5')
    return(NULL)
  }
  # Voronoi of the design:
  
  if (designType=='square'){
    if(!is.null(dist_intercrop) | !is.na(dist_intercrop)){
      print('dist_intercrop is not considered in square design')
      return(NULL)}
    
    x1=dist_inter/2
    y1=dist_intra/2
    xmax=x1+dist_inter/2
    ymax=y1+dist_intra/2
    
    voronoi_plot=data.frame(x= x1,
                            y= y1,
                            xmin= 0,xmax=xmax,
                            ymin= 0, ymax= ymax)
    
  }
  
  # ### test for estimating differences du to rotation in Archimed 
  # if (designType %in% c('square_bis')){
  #   if(!is.null(dist_intercrop)){
  #     print('dist_intercrop is not considered in square design')
  #     return(NULL)}
  #   
  #   x1=dist_inter/2
  #   y1=dist_intra/2
  #   xmax=x1+dist_inter/2
  #   ymax=y1+dist_intra/2
  #   
  #   voronoi_plot=data.frame(x= y1,
  #                           y= x1,
  #                           xmin= 0,xmax=ymax,
  #                           ymin= 0, ymax= xmax)
  #   
  # }
  
  if (designType=='quincunx'){
    if(!is.null(dist_intercrop) | !is.na(dist_intercrop)){
      print('dist_intercrop is not considered in quincunx design')
      return(NULL)}
    
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
  
  # if (designType=='quincunx_bis'){
  #   if(!is.null(dist_intercrop)){
  #     print('dist_intercrop is not considered in quincunx design')
  #     return(NULL)}
  #   
  #   x1=dist_inter/2
  #   y1=dist_intra/4
  #   x2=dist_inter/2+dist_inter
  #   y2=dist_intra/4+dist_intra/2
  #   xmax=x2+dist_inter/2
  #   ymax=y2+dist_intra/4
  #   
  #   voronoi_plot=data.frame(x= c(y1,y2),
  #                           y= c(x1,x2),
  #                           xmin= 0,xmax=ymax,
  #                           ymin= 0, ymax= xmax)
  # }
  
  if (designType=='quincunx2'){
    if(is.null(dist_intercrop)  | is.na(dist_intercrop)){
      print('please provide dist_intercrop in quincunx2 design')
      return(NULL)}
    
    x1=dist_inter/2
    y1=dist_intra/4
    x2=dist_inter/2+dist_intercrop
    y2=dist_intra/4+dist_intra/2
    xmax=dist_inter+dist_intercrop
    ymax=dist_intra
    
    voronoi_plot= data.frame(x= c(x1,x2),
                                    y= c(y1,y2),
                                    xmin= 0,xmax=xmax,
                                    ymin= 0, ymax= ymax)
    
  }
  
  if (designType=='square2'){
    if(is.null(dist_intercrop) | is.na(dist_intercrop)){
      print('please provide dist_intercrop in square2 design')
      return(NULL)}
    
    
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
    if(is.null(dist_intercrop) | is.na(dist_intercrop)){
      print('please provide dist_intercrop in quincunx3 design')
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
  
  
  if (designType=='quincunx4'){
    if(is.null(dist_intercrop) | is.na(dist_intercrop)){
      print('please provide dist_intercrop in quincunx4 design')
      return(NULL)}
    
    x1=dist_inter/2
    y1=dist_intra/4
    x2=x1+dist_inter
    y2=y1+dist_intra/2
    x3=x2+dist_intercrop
    y3=y1
    x4=x3+dist_inter
    y4=y2
    xmax=x4+dist_inter/2
    ymax=y4+dist_intra/4
    
    voronoi_plot= data.frame(x= c(x1,x2,x3,x4),
                                       y= c(y1,y2,y3,y4),
                                       xmin= 0, xmax= xmax,
                                       ymin= 0, ymax= ymax)
    
  }
  
  if (designType=='quincunx5'){
    if(is.null(dist_intercrop)| is.na(dist_intercrop)){
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
  
  
  if (designType %in% c('square','square_bis','quincunx_bis','quincunx')){
    dist_intercrop=0}
      # number of raow and columns in 1 ha
    repRows=ceiling(lim/(dist_inter+dist_intercrop))
    repCol=ceiling(lim/dist_intra)
    
  # number of raow and columns in 1 ha
  
  
  # Matrix of the design (each cell is a Voronoi):
  mat_plot= expand.grid(repRows= 1:repRows, repCol= 1:repCol)
  
  # density
  density=floor(nrow(voronoi_plot)/(unique(voronoi_plot$xmax)*unique(voronoi_plot$ymax))*10000)
  
  
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
  
  # plot:
  

  if(orientation=='NS'){
  plot_bounds=
    design%>%
    ggplot2::ggplot(ggplot2::aes(x= x, y= y))+
    ggplot2::geom_point(shape=8,col='forestgreen',size=pointSize)+
    ylab('Intra row distance (m)')+
    xlab('Inter row distance (m)')+
    xlim(c(0,lim))+
    ylim(c(0,lim))+
    ggtitle(paste(density,' plants.ha-1'))+
    coord_equal()+
    myTheme
  }
  if(orientation=='EW'){
    plot_bounds=design%>%
      ggplot2::ggplot(ggplot2::aes(x= y, y= x))+
      ggplot2::geom_point(shape=8,col='forestgreen',size=pointSize)+
      xlab('Intra row distance (m)')+
      ylab('Inter row distance (m)')+
      xlim(c(0,lim))+
      ylim(c(0,lim))+
      ggtitle(paste(density,' plants.ha-1'))+
      coord_equal()+
      myTheme
      
      
  }
  
  if (replanting==T){
    
    oldDesign=data.table::fread(file = '0-data/oldDeisgn9x9.csv') %>% 
      data.frame()%>%
      mutate(x=x-dist_inter/2,
             y=y-dist_intra/2)
    
    if(orientation=='NS'){
    plot_bounds=ggplot()+
      geom_point(data=oldDesign,aes(x= x, y= y,col='old planting',shape='old planting'),size=pointSize/2)+
      geom_point(data=design,aes(x= x, y= y,col='replanting',shape='replanting'),size=pointSize)+
      ylab('Intra row distance (m)')+
      xlab('Inter row distance (m)')+
      xlim(c(0,lim))+
      ylim(c(0,lim))+
      ggtitle(paste(density,' plants.ha-1'))+
      coord_equal()+
      myTheme+
      scale_color_manual(values = c('old planting'='lightgrey',replanting='forestgreen'),name='')+
      scale_shape_manual(values = c('old planting'=1,'replanting'=8),name='')+
      theme(legend.position='bottom')
    }
    if(orientation=='EW'){
      plot_bounds=ggplot()+
        geom_point(data=oldDesign,aes(x= y, y= x,col='old planting',shape='old planting'),size=pointSize/2)+
        geom_point(data=design,aes(x= y, y= x,col='replanting',shape='replanting'),size=pointSize)+
        xlab('Intra row distance (m)')+
        ylab('Inter row distance (m)')+
        xlim(c(0,lim))+
        ylim(c(0,lim))+
        ggtitle(paste(density,' plants.ha-1'))+
        coord_equal()+
        myTheme+
        scale_color_manual(values = c('old planting'='lightgrey',replanting='forestgreen'),name='')+
        scale_shape_manual(values = c('old planting'=1,'replanting'=8),name='')+
        theme(legend.position='bottom')
      
      
    }
    
  }
  

  
  im=readPNG("north.png")
  
  visu=ggdraw() +
    draw_plot(plot_bounds) +
    draw_image(im,x = 0.4,y = 0.4,scale=0.1)
  
  # result to export scene pattern
  if (orientation=='NS'){
    result=
      voronoi_plot%>%
      dplyr::mutate(z= 0.0, scale= 1.0,
                    inclinationAzimut= 0.0, inclinationAngle= 0.0,
                    stemTwist= twist)
  }
 
  if (orientation=='EW'){
    result=
      voronoi_plot%>%
      dplyr::mutate(z= 0.0, scale= 1.0,
                    inclinationAzimut= 0.0, inclinationAngle= 0.0,
                    stemTwist= twist)
  }
  
  list(design=design,result= result, plot= visu,density=density)
  
}



### save the conventional design for visualizing old palms in replanting 
# l=9
# h=sqrt(3*l**2/4)
# dist_intra=l
# dist_intercrop=NULL
# dist_inter=h
# 
# oldDesign=generate_design(dist_intra = dist_intra,dist_intercrop = NULL,dist_inter = h,designType = 'quincunx',orientation = 'NS')$design

### save

# fwrite(x =oldDesign,file = '0-data/oldDeisgn9x9.csv')
  



