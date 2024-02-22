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
#' @param designType type of design (square,quincunx,quincunx2,quincunx3,quincunx4,quincunx5)
#' @param orientation orientation of the scene ('NS': North-South or 'EW': East-West)
#' @param pointSize size of point in the plot
#'
#' @return
#' @export
#'
#' @examples
generate_design=function(dist_intra=NULL,dist_inter=NULL,dist_intercrop=NULL,designType=NULL,orientation=orientation,pointSize=5){

  # l=9.21
  # h=sqrt(3*l**2/4)
  # dist_intra=l
  # dist_intercrop=2*h
  # dist_inter=h
  # designType='square'

  if (!designType %in% c('square','quincunx','quincunx2','quincunx3','quincunx4','quincunx5')){
    print('please select a designType among square quincunx quincunx2  quincunx3 quincunx4 quincunx5')
    return(NULL)
  }
  # Voronoi of the design:
  
  if (designType=='square'){
    if(!is.null(dist_intercrop)){
      print('dist_intercrop is not considered in square design')}
    
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
    if(is.null(dist_intercrop)){
      print('dist_intercrop is not considered in quinconx design')
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
  
  if (designType=='quincunx2'){
    if(is.null(dist_intercrop)){
      print('please provide dist_intercrop in quinconx2 design')
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
 
  if (designType=='quincunx3'){
    if(is.null(dist_intercrop)){
      print('please provide dist_intercrop in quinconx3 design')
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
    if(is.null(dist_intercrop)){
      print('please provide dist_intercrop in quinconx4 design')
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
    if(is.null(dist_intercrop)){
      print('please provide dist_intercrop in quinconx5 design')
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
  
  
  if (designType %in% c('square','quincunx')){
    dist_intercrop=0}
      # number of raow and columns in 1 ha
    repRows=ceiling(100/(dist_inter+dist_intercrop))
    repCol=ceiling(100/dist_intra)
    
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
  


  plot_bounds=
    design%>%
    ggplot2::ggplot(ggplot2::aes(x= x, y= y))+
    ggplot2::geom_point(shape=8,col='forestgreen',size=pointSize)+
    ylab('Intra row distance (m)')+
    xlab('Inter row distance (m)')+
    xlim(c(0,100))+
    ylim(c(0,100))+
    ggtitle(paste(density,' plants.ha-1'))+
    coord_equal()+
    myTheme
  
  if(orientation=='EW'){
    plot_bounds=design%>%
      ggplot2::ggplot(ggplot2::aes(x= y, y= x))+
      ggplot2::geom_point(shape=8,col='forestgreen',size=pointSize)+
      xlab('Intra row distance (m)')+
      ylab('Inter row distance (m)')+
      xlim(c(0,100))+
      ylim(c(0,100))+
      ggtitle(paste(density,' plants.ha-1'))+
      coord_equal()+
      myTheme
      
      
  }
  
  im=readPNG("north.png")
  
  visu=ggdraw() +
    draw_plot(plot_bounds) +
    draw_image(im,x = 0.4,y = 0.4,scale=0.1)
  
  # result to export scene pattern
  result=
    voronoi_plot%>%
    dplyr::mutate(z= 0.0, scale= 1.0,
                  inclinationAzimut= 0.0, inclinationAngle= 0.0,
                  stemTwist= 0.0)
  
  list(design=design,result= result, plot= visu,density=density)
  
}


# generate_design(dist_intra = 5,dist_intercrop = 0,dist_inter = 15,designType = 'square',orientation = 'EW')
# generate_design(dist_intra = 5,dist_intercrop = 0,dist_inter = 15,designType = 'square',orientation = 'NS')
# 
# generate_design(dist_intra = 9,dist_intercrop = 12,dist_inter = 5,designType = 'quincunx2',orientation = 'NS')
# generate_design(dist_intra = 9,dist_intercrop = 12,dist_inter = 5,designType = 'quincunx3',orientation = 'EW')

# inputs ------------------------------------------------------------------

##### quincunx designs



##### design 1/3--> double rangée
# 
# 
# dist_intra=l
# dist_intercrop=2*h
# dist_inter=h
# 
# x1=dist_inter/2
# y1=dist_intra/4
# x2=dist_inter/2+dist_intercrop
# y2=dist_intra/4+dist_intra/2
# xmax=dist_inter+dist_intercrop
# ymax=dist_intra
# 
# voronoi_plot_double= data.frame(x= c(x1,x2),
#                          y= c(y1,y2),
#                          xmin= 0,xmax=xmax,
#                          ymin= 0, ymax= ymax)
# 
# 
# generate_design(voronoi_plot =voronoi_plot_double )
# 
# 
# ### design 1/4--> triple rangée
# 
# 
# x1=dist_inter/2
# y1=dist_intra/4
# x2=x1+dist_inter
# y2=y1+dist_intra/2
# x3=x2+dist_intercrop
# y3=y2
# xmax=x3+dist_inter/2
# ymax=y3+dist_intra/4
# 
# voronoi_plot_triple= data.frame(x= c(x1,x2,x3),
#                            y= c(y1,y2,y3),
#                            xmin= 0, xmax= xmax,
#                            ymin= 0, ymax= ymax)
# 
# 
# generate_design(voronoi_plot =voronoi_plot_triple )
# 
# ##### design 2/6 ou 1/5--> quadruple rangées
# 
# dist_intercrop=14
# 
# x1=dist_inter/2
# y1=dist_intra/4
# x2=x1+dist_inter
# y2=y1+dist_intra/2
# x3=x2+dist_intercrop
# y3=y1
# x4=x3+dist_inter
# y4=y2
# xmax=x4+dist_inter/2
# ymax=y4+dist_intra/4
# 
# voronoi_plot_quadruple= data.frame(x= c(x1,x2,x3,x4),
#                            y= c(y1,y2,y3,y4),
#                            xmin= 0, xmax= xmax,
#                            ymin= 0, ymax= ymax)
# 
# generate_design(voronoi_plot =voronoi_plot_quadruple )
# 
#   
# ##### design 1/6 --> quintuple rangées
# x1=dist_inter/2
# y1=dist_intra/4
# x2=x1+dist_inter
# y2=y1+dist_intra/2
# x3=x2+dist_intercrop
# y3=y2
# x4=x3+dist_inter/2
# y4=y1
# x5=x4+dist_inter
# y5=y3
# xmax=x5+dist_inter/2
# ymax=y5+dist_intra/4
# 
# voronoi_plot_quintuple= data.frame(x= c(x1,x2,x3,x4,x5),
#                                    y= c(y1,y2,y3,y4,y5),
#                                    xmin= 0, xmax= xmax,
#                                    ymin= 0, ymax= ymax)
# 
# generate_design(voronoi_plot =voronoi_plot_quintuple )
