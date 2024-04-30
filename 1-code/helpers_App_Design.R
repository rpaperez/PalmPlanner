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
#' @param lim limit of the area for plotting map (m)
#' @param twist twist/rotation of the palm stem in the ops file (in degree)
#' 
#' @return
#' @export
#'
#' @examples
plot_design=function(dist_intra=NULL,dist_inter=NULL,dist_intercrop=NULL,designType=NULL,orientation=orientation,twist=twist,pointSize=5,lim=50){
  
  # l=9.21
  # h=sqrt(3*l**2/4)
  # dist_intra=l
  # dist_intercrop=2*h
  # dist_inter=h
  # designType='square'
  # pointSize=3
  
  if(dist_intra<0){
    print('Intra row distance must be > 0')
    return(NULL)
  }
  if(dist_inter<0){
    print('Inter row distance must be > 0')
    return(NULL)
  }
  if(dist_intercrop<0){
    print('Intercropping distance must be >= 0')
    return(NULL)
  }
  
  
  if (!designType %in% c('square','square2','quincunx','quincunx2','quincunx3','quincunx4','quincunx5')){
    print('please select a designType among square quincunx quincunx3 quincunx4 quincunx5')
    return(NULL)
  }
  # Voronoi of the design:
  
  if (designType=='square'){
    if(dist_intercrop>0){
      print('Intercropping distance must be 0 m for square design')
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
      print('Intercropping distance must be 0m for quincunx design')
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
      print('Intercropping distance must > 0 m for quincunx2 design')
      return(NULL)
    }
    
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
    
    if(dist_intercrop==0){
      print('Intercropping distance must > 0 m for square2 design')
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
    if(dist_intercrop==0){
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
  
  
  # 
  # im=readPNG("north.png")
  
  visu=plot_bounds
  
  # # result to export scene pattern
  # if (orientation=='NS'){
  #   result=
  #     voronoi_plot%>%
  #     dplyr::mutate(z= 0.0, scale= 1.0,
  #                   inclinationAzimut= 0.0, inclinationAngle= 0.0,
  #                   stemTwist= twist)
  # }
  # 
  # if (orientation=='EW'){
  #   result=
  #     voronoi_plot%>%
  #     dplyr::mutate(z= 0.0, scale= 1.0,
  #                   inclinationAzimut= 0.0, inclinationAngle= 0.0,
  #                   stemTwist= twist)
  # }
  
  list(design=design, voronoi_plot=voronoi_plot,plot= visu,density=density)
  
}



# add intercrop design ----------------------------------------------------

#' Title
#'
#' @param dist_intra distance within row of palm trees (m)
#' @param dist_inter distance between rows of palm trees (m)
#' @param dist_intercrop distance between multiple rows of palm trees (m), for intercropping 
#' @param designType type of design for oil palm (square,square2,quincunx,quincunx2,quincunx3,quincunx4,quincunx5)
#' @param orientation orientation of the scene ('NS': North-South or 'EW': East-West)
#' @param pointSize size of point in the plot
#' @param lim limit of the area for plotting map (m)
#' @param twist twist/rotation of the palm stem in the ops file (in degree)
#' @param I_dist_intra distance within row of intercrop (m)
#' @param I_dist_inter distance between rows of intercrop (m)
#' @param I_designType type of design for intercrop (square,square2,quincunx,quincunx2,quincunx3,quincunx4,quincunx5)
#'
#' @return
#' @export
#'
#' @examples
plot_intercrop=function(dist_intra=NULL,dist_inter=NULL,dist_intercrop=NULL,designType=NULL,orientation=orientation,twist=twist,pointSize=5,lim=50,I_dist_intra=NULL,I_dist_inter=NULL,I_designType=NULL){
  
  # I_dist_intra=2
  # I_dist_inter=3
  # I_designType='quincunx3'
  
  # d_intercrop=12
  # d_intra=10
  # lim=100
  # d_inter=7
  # designType='quincunx5'
  
  if(designType %in% c('square','quincunx')){
    dist_intercrop=0}
  
  a=plot_design(dist_intra=dist_intra,dist_inter=dist_inter,dist_intercrop=dist_intercrop,designType=designType,orientation=orientation,twist=twist,pointSize=pointSize,lim=lim)
  
  # print(nrow(a$voronoi_plot))
  
  ### get the middle and distance between intercrop area
  
  if(designType %in% c('square','quincunx')){
    middle=min(a$voronoi_plot$x)+dist_inter/2
    D_intercrop=dist_inter
  }
  
  
  if(designType %in% c('square2','quincunx2')){
    middle=min(a$voronoi_plot$x)+dist_intercrop/2
    D_intercrop=dist_intercrop+(nrow(a$voronoi_plot)-1)*dist_inter
  }
  
  if(designType %in% c('quincunx3','quincunx4','quincunx5')){
    middle=min(a$voronoi_plot$x)+dist_inter+dist_intercrop/2
    D_intercrop=dist_intercrop+(nrow(a$voronoi_plot)-1)*dist_inter
  }
  
  
  
  if(I_designType %in% c('square','quincunx')){
    dist_intercrop2=D_intercrop
    b=plot_design(dist_intra=I_dist_intra,dist_inter=dist_intercrop2,dist_intercrop=0,designType=I_designType,orientation=orientation,twist=twist,pointSize=pointSize,lim=2*lim)
    interCrop=b$design %>% 
      mutate(x=x-min(x)+middle,y=y-min(y))
  }
  
  if(I_designType %in% c('square2','quincunx2','quincunx3','quincunx4','quincunx5')){
    dist_intercrop2=D_intercrop-(nrow(a$voronoi_plot)-1)*I_dist_inter
    b=plot_design(dist_intra=I_dist_intra,dist_inter=I_dist_inter,dist_intercrop=dist_intercrop2,designType=I_designType,orientation=orientation,twist=twist,pointSize=pointSize,lim=2*lim) 
  
    interCrop=b$design %>% 
      filter(x>b$design$x[nrow(b$voronoi_plot)-1]) %>% 
      mutate(x=x-min(x)+middle-(nrow(b$voronoi_plot)-1)*I_dist_inter/2,y=y-min(y))
  }
  
  
  
  visu=a$design %>% 
    ggplot(aes(x=x,y=y))+
    geom_point()+
    geom_point(data=interCrop,aes(x=x,y=y,col='intercrop'))+
    xlim(c(0,lim))+
    ylim(c(0,lim))
  
  return(visu)
  
}


# test --------------------------------------------------------------------



dist_intercrop=12
dist_intra=10
dist_inter=7
designType='quincunx5'
I_dist_intra=2
I_dist_inter=3
I_designType='quincunx'
lim=100
orientation='NS'
twist=0
pointSize=3

plot_intercrop(dist_intra =dist_intra,dist_inter =dist_inter,designType =designType,dist_intercrop =dist_intercrop,orientation = orientation,twist = twist,pointSize =pointSize,lim = lim, I_dist_intra = I_dist_intra,I_dist_inter =I_dist_inter,I_designType = I_designType)

