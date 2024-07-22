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


# nbRow=10
# nbCol=10

#' Function to generate OPS  and visualization of the designs
#'
#' @param dist_intra distance within row of palm trees (m)
#' @param dist_inter distance between rows of palm trees (m)
#' @param dist_intercrop distance between multiple rows of palm trees (m), for intercropping 
#' @param designType type of design (square,square2,quincunx,quincunx2,quincunx3,square3,quincunx4,square4,quincunx5,square5)
#' @param orientation orientation of the scene ('NS': North-South or 'EW': East-West)
#' @param pointSize size of point in the plot
#' @param lim limit of the area for plotting map (m)
#' 
#' @return
#' @export
#'
#' @examples
plot_design=function(dist_intra=NULL,dist_inter=NULL,dist_intercrop=NULL,designType=NULL,orientation=orientation,pointSize=5,lim=50){
  
  # l=9.21
  # h=sqrt(3*l**2/4)
  # dist_intra=l
  # dist_intercrop=2*h
  # dist_inter=h
  # designType='square'
  # pointSize=3

  
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
    
    # x1=dist_inter/2
    # y1=dist_intra/4
    # x2=dist_inter/2+dist_intercrop
    # y2=dist_intra/4+dist_intra/2
    # xmax=dist_inter+dist_intercrop
    # ymax=dist_intra
    
    # voronoi_plot= data.frame(x= c(x1,x2),
    #                          y= c(y1,y2),
    #                          xmin= 0,xmax=xmax,
    #                          ymin= 0, ymax= ymax)
    
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
    # voronoi_plot= data.frame(x= c(x1,x2,x3,x4),
    #                          y= c(y1,y2,y3,y4),
    #                          xmin= 0, xmax= xmax,
    #                          ymin= 0, ymax= ymax)
    
    x1=dist_intercrop/2
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
    x7=x6+dist_inter
    y7=y2
    x8=x7+dist_inter
    y8=y1
    
    xmax=x8+dist_intercrop/2
    ymax=y7+dist_intra/4

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

  # im=readPNG("north.png")
  
  # result to export scene pattern
  
    scene=
      voronoi_plot%>%
      dplyr::mutate(z= 0.0, scale= 1.0,
                    inclinationAzimut= 0.0, inclinationAngle= 0.0,
                    stemTwist= 0)


  list(design=design, voronoi_plot=voronoi_plot,plot= plot_bounds,density=density,scene=scene)
  
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
#' @param orientation orientation of the scene ('NS': North-South or 'EW': East-West)
#' @param pointSize size of point in the plot
#' @param lim limit of the area for plotting map (m)
#' @param I_dist_intra distance within row of intercrop (m)
#' @param I_dist_inter distance between rows of intercrop (m)
#' @param I_designType type of design for intercrop (square,square2,quincunx,quincunx2,quincunx3,quincunx4,quincunx5)
#'
#' @return
#' @export
#'
#' @examples
design_intercrop=function(dist_intra=NULL,dist_inter=NULL,dist_intercrop=NULL,designType=NULL,orientation=orientation,pointSize=5,lim=50,I_dist_intra=NULL,I_dist_inter=NULL,I_designType=NULL){
  
  ### get distance between intercrop edge and palms row
  dist_edge=(dist_intercrop-sizeDesign[sizeDesign==I_designType,]$interLines*I_dist_inter)/2

  if(designType %in% c('square','quincunx')){
    dist_intercrop=0
    dist_edge=(dist_inter-sizeDesign[sizeDesign==I_designType,]$interLines*I_dist_inter)/2
    if(I_designType %in% c('square','quincunx')){
      dist_edge=dist_inter/2
    }
  }
  
  print(paste('dist edge=',dist_edge))
  # print(dist_edge)
  if((dist_edge)<0){
    print('!!! intercroping area is larger than intercropping space between palm rows')
    return(NULL)
  }
  
  ### first generate palm design
  a=plot_design(dist_intra=dist_intra,dist_inter=dist_inter,dist_intercrop=dist_intercrop,designType=designType,orientation=orientation,pointSize=pointSize,lim=lim)
  
  #### then generate intercrops design
  
  ### get distance between 2 consecutive intercropping areas (D_intercrops)
  D_intercrops=2*dist_edge+sizeDesign[sizeDesign==designType,]$interLines*dist_inter
  

  if(I_designType %in% c('square','quincunx')){
    I_dist_inter=D_intercrops
    D_intercrops=0
    print(paste('I_dist_inter is fixed to: ',I_dist_inter,'m'))
  }
  

  b=plot_design(dist_intra=I_dist_intra,dist_inter=I_dist_inter,dist_intercrop=D_intercrops,designType=I_designType,orientation=orientation,pointSize=pointSize,lim=2*lim) 
  
  ### keep full intercropping area (remove first rows)
  I_removeL=min(2,sizeDesign[sizeDesign$designType==I_designType,]$firstLines-1) ### lines to remove
  # print(paste('I_remove====',I_removeL))
  I_start=ifelse(I_removeL>0,unique(b$design$x)[I_removeL],0)
  # print(paste('I_start====',I_start))
  
  offSetL=max(1,min(2,sizeDesign[sizeDesign$designType==designType,]$firstLines-1)) ### first lines  in palm design to get the offset of intercropping area
  # print(paste('offSetL=',offSetL))
  
  interCrop=  b$design %>% 
    filter(x>I_start) %>% 
    mutate(x=x-min(x)+unique(a$design$x)[offSetL]+dist_edge,y=y-min(y)) ### add the offset (dist_edge)
  
  visu=a$design %>% 
    ggplot(aes(x=x,y=y))+
    geom_point(aes(col= 'palms'),shape=8,size=pointSize)+
    geom_point(data=interCrop,aes(x=x,y=y,col='intercrop'),size=pointSize-1)+
    xlim(c(0,lim))+
    ylim(c(0,lim))+
    scale_color_manual(name='',values = c('palms'='forestgreen','intercrop'='black'))
  
  result=list(designPalm=a$design,density=b$density,designI=interCrop,plot=visu)
  return(result)
}

# CREATE OPS --------------------------------------------------------------


#' Function to generate ops
#'
#' @param opfname name of the opf to call 
#' @param dist_intra distance within row of palm trees (m)
#' @param dist_inter distance between rows of palm trees (m)
#' @param dist_intercrop distance between multiple rows of palm trees (m), for intercropping 
#' @param designType type of design
#' @param plant_model model input for archimed
#' @return ops file
#' @export
#'
#' @examples
create.ops=function(opfname='opfname',dist_inter= NULL, dist_intra= NULL,dist_intercrop=NULL,designType=NULL,orientation=orientation,writeOPS=T,pathOPS=NULL,plant_model='elaeis'){
  
  # opfname='opfname'
  # dist_inter=12
  # dist_intra=12
  # dist_intercrop=0
  # designType='square'
  # orientation='NS'
  # plant_model='elaeis'
  # pathOPS='./2-outputs/Run_simu/ops/'
  
  ### generate the design
  
  des=plot_design(dist_inter =dist_inter, dist_intra=dist_intra,dist_intercrop =dist_intercrop,designType = designType,orientation = orientation,pointSize = 3)$scene
  
  
  xmin=unique(des$xmin)
  xmax=unique(des$xmax)
  ymin=unique(des$ymin)
  ymax=unique(des$ymax)
  zmin=0
  
  
  ###generate ligne of config file for each tree
  
  table=NULL
  for (t in 1:nrow(des)){
    tableSub=paste(1,t,paste('opf/',opfname,'.opf',sep=''),des$x[t],des$y[t],des$z[t],des$scale[t],des$inclinationAzimut [t],des$inclinationAngle[t],des$stemTwist[t],sep='	')
    table=rbind(table,tableSub)
  }
  
  if (nrow(des)==1){
    ops.file=c(
      paste('# T xmin ymin zmin xmax ymax flat'),
      paste('T', xmin, ymin, zmin, xmax,ymax,'flat'),
      paste('#[Archimed]',plant_model),
      paste('#sceneId objectId FilePath x y z scale inclinationAzimut inclinationAngle rotation'),
      paste(table[1,],collapse=' ')
    )
  }
  
  if (nrow(des)==2){
    ops.file=c(
      paste('# T xmin ymin zmin xmax ymax flat'),
      paste('T', xmin, ymin, zmin, xmax,ymax,'flat'),
      paste('#[Archimed]',plant_model),
      paste('#sceneId objectId FilePath x y z scale inclinationAzimut inclinationAngle rotation'),
      paste(table[1,]),
      paste(table[2,],collapse=' ')
    )
  }
  
  if (nrow(des)==3){
    ops.file=c(
      paste('# T xmin ymin zmin xmax ymax flat'),
      paste('T', xmin, ymin, zmin, xmax,ymax,'flat'),
      paste('#[Archimed]',plant_model),
      paste('#sceneId objectId FilePath x y z scale inclinationAzimut inclinationAngle rotation'),
      paste(table[1,]),
      paste(table[2,],collapse=' '),
      paste(table[3,],collapse=' ')
    )
  }
  if (nrow(des)==4){
    ops.file=c(
      paste('# T xOrigin yOrigin zOrigin xSize ySize flat'),
      paste('T', xmin, ymin, zmin, xmax,ymax,'flat'),
      paste('#[Archimed]',plant_model),
      paste('#sceneId plantId plantFileName x y z scale inclinationAzimut inclinationAngle stemTwist'),
      paste(table[1,]),
      paste(table[2,],collapse=' '),
      paste(table[3,],collapse=' '),
      paste(table[4,],collapse=' ')
    )
  }
  
  if (nrow(des)==5){
    ops.file=c(
      paste('# T xOrigin yOrigin zOrigin xSize ySize flat'),
      paste('T', xmin, ymin, zmin, xmax,ymax,'flat'),
      paste('#[Archimed]',plant_model),
      paste('#sceneId plantId plantFileName x y z scale inclinationAzimut inclinationAngle stemTwist'),
      paste(table[1,]),
      paste(table[2,],collapse=' '),
      paste(table[3,],collapse=' '),
      paste(table[4,],collapse=' '),
      paste(table[5,],collapse=' ')
    )
  }
  
  if (nrow(des)>5){
    print('more than 5 opfs are not implemented')
    return(NULL)
  }
  if (writeOPS==T){
    
    if (designType %in% c('square','quincunx')){
      write(ops.file,file= paste0(pathOPS,'/',designType,'_inter',dist_inter,'_intra',dist_intra,'_',opfname,'.ops')) 
      
    }
    if (!(designType %in% c('square','quincunx'))){
      write(ops.file,file= paste0(pathOPS,'/',designType,'_inter',dist_inter,'_intra',dist_intra,'_intercrop',dist_intercrop,'_',opfname,'.ops')) ###wrtie the ops
    }
    
  }
  return(ops.file)
  
}

# RUN SIMU ----------------------------------------------------------------

#' Run VPalm and Archimed simulation
#'@param MAP: months after planting
#'@param d_inter: distance between rows of palm trees
#'@param d_intra: distance within rows of palm trees
#'@param d_intercrop: distance for intercrop
#' @param designType type of design (square,square2,quincunx,quincunx2,quincunx3,square3,quincunx4,square4,quincunx5,square5)
#' @param pathVpalmParam path to paramFileName
#' @param  path_designs path to save planting designs
#' @param pathArchimed path to archimed jar
#' @param pathVpalmJar path to Vpalm Jar
#' @param pathOpf Path where opf is written
#' @param pathOPS path where ops is written
#' @param run_photosynthesis run phototsynthesis (T or F)
#' @param opfStepExport step of simulation when the opf is exported (numeric 0-28)
#' @param overwrite overwrite existing simulation
#' @param orientation orientation of the scene (NS: North-South or EW: East-West)
#'@param meteoFile meteoFIle for running simulations
#'#'@param paramFileName paramFileName
#'@param plant_model model input for archimed
#'
#'
#' @return
#' @export
#'
#' @examples
RunSimu=function(paramFileName=paramFileName,d_inter=d_inter,d_intra=d_intra,d_intercrop=d_intercrop,designType=designType,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=14,overwrite=F,orientation='NS',meteoFile='meteoCampecheFormated.csv',plant_model='elaeis'){
  
  # plant_model='elaeis'
  # orientation='NS'
  # meteoFile='meteoCampecheFormated.csv'
  # paramFileName='Mockup_seed1_MAP_180'
  # d_inter=6.8
  # d_intra=9
  # d_intercrop=16.6
  # designType='quincunx2'
  # path_designs='./2-outputs/Run_simu/planting_designs/'
  # pathVpalmParam='./2-outputs/Generate_VPalm_param/'
  # pathArchimed='./1-code/archimed-phys.jar'
  # pathVpalmJar='./1-code/vpalm_biomech.jar'
  # pathOpf='./2-outputs/Run_simu/ops/opf/'
  # pathOPS='./2-outputs/Run_simu/ops/'
  # onlyArchimed=F
  # opfStepExport=6
  # run_photosynthesis=F
  # overwrite=T
  
  if (designType %in% c('square','quincunx')){
    design_name=paste0(designType,'_inter',d_inter,'_intra',d_intra)
    
  }
  if (!(designType %in% c('square','quincunx'))){
    design_name=paste0(designType,'_inter',d_inter,'_intra',d_intra,'_intercrop',d_intercrop)
  }
  
  folders=list.dirs(path ='./2-outputs/Run_simu/output',full.names = T)
  
  foldSim=paste0('./2-outputs/Run_simu/output/',design_name,'_',paramFileName,'_',orientation)
  
  if (foldSim %in% folders & overwrite==F){
    print(paste(foldSim,'already exists, if you want to overwrite change overwrite argument in the RunSimu function'))
  }
  
  if (!(foldSim %in% folders)| overwrite==T){
    
    print(paste("Running simulation:",foldSim))
    
    
    # !!!!! to recode ---------------------------------------------------------
    
    
    # generate Vpalm Parameters -----------------------------------------------
    
    # if(!paste0(paramFileName,'.txt') %in% list.files(path = pathVpalmParam,pattern = 'txt')){
    #   print(paste0('writting VPalm parameter file: ',pathVpalmParam,paramFileName,'.txt'))
    #   Generate_Vpalm_param(MAP_requested =MAP)
    # }
    # 
    # # generate opf ------------------------------------------------------------
    # 
    # if(!paste0(paramFileName,'.opf') %in% list.files(path = pathOpf,pattern = 'opf')){
    #   print(paste0('creating opf: ',pathOpf,paramFileName,'.opf'))
    #   system(command = paste0('java -jar ',pathVpalmJar,' ',pathVpalmParam,paramFileName,'.txt',' ',pathOpf,paramFileName,'.opf'))
    # }
    
    
    # generate ops ------------------------------------------------------------
    print('writting ops')
    create.ops(opfname =paramFileName ,dist_inter = d_inter,dist_intra = d_intra,designType = designType,dist_intercrop =d_intercrop,orientation =orientation,  writeOPS = T,pathOPS = pathOPS,plant_model=plant_model)
    
    ## save planting design
    print('saving planting design')
    p_design=plot_design(dist_inter =d_inter, dist_intra=d_intra,dist_intercrop =d_intercrop,designType = designType ,orientation = orientation,pointSize = 3)$scene
    
    data.table::fwrite(x=p_design,file = paste0(path_designs,design_name,'.csv'))
    
    # generate config file ----------------------------------------------------
    
    ##"load template
    
    configYml=read_yaml(file =  '0-data/Archimed_inputs/config_template.yml')
    meteo=data.table::fread(paste0('2-outputs/Run_simu/',meteoFile),skip=4,header=T)
    configYml$models[1]=paste0('models/',plant_model,'.yml')
    configYml$meteo=meteoFile
    configYml$meteo_range=paste0('1, ',nrow(meteo))
    # configYml$export_ops=opfStepExport
    
    ## change names & dirs
    opsName=paste0(design_name,'_',paramFileName,'.ops')
    configYml$scene=paste0('ops/',opsName)
    configYml$simulation_directory=paste0(str_remove(string = opsName,pattern = '.ops'),'_',orientation)
    configYml$export_ops=as.character(round(opfStepExport))
    
    if (!(orientation %in% c('NS','EW'))){
      print('orientation must be NS or EW')
    }
    
    if (orientation=='EW'){
      configYml$scene_rotation=90
    }
    
    
    if (run_photosynthesis==T){
      # Absorbed light
      configYml$component_variables$Ra_PAR_0_f=T
      configYml$component_variables$Ra_NIR_0_f=T
      configYml$component_variables$Ra_PAR_0_q=T
      configYml$component_variables$Ra_NIR_0_q=T
      configYml$component_variables$Ra_PAR_f=T
      configYml$component_variables$Ra_NIR_f=T
      configYml$component_variables$Ra_TIR_f=T
      configYml$component_variables$Ra_PAR_q=T
      configYml$component_variables$Ra_NIR_q=T
      configYml$component_variables$Ra_TIR_q=T
      # Assimilation
      configYml$component_variables$An_f=T
      configYml$component_variables$An_q=T
      configYml$component_variables$Gs=T
      # Energy
      configYml$component_variables$H_q=T
      configYml$component_variables$H_f=T
      configYml$component_variables$LE_q=T
      configYml$component_variables$LE_f=T
      configYml$component_variables$Tr_f=T
      configYml$component_variables$Tr_q=T
      configYml$component_variables$T=T
    }
    
    pathConfig=paste0('2-outputs/Run_simu/',str_replace(string = opsName,pattern = '.ops',replacement = paste0('_',orientation,'.yml'))) 
    
    yaml::write_yaml(x =  configYml,file =pathConfig)
    
    debut=Sys.time()
    system(command =paste0('java -jar ',pathArchimed,' ',pathConfig))
    
    
    print(paste("compute time :",difftime(time1 = Sys.time(),time2 = debut,units = "mins"),'mins'))
  }
}

# test --------------------------------------------------------------------

# 
# 
# dist_intercrop=10
# dist_intra=5
# dist_inter=10
# designType='quincunx3'
# I_dist_intra=5
# I_dist_inter=10
# I_designType='quincunx'
# lim=50
# orientation='NS'
# pointSize=3
# #
# #
# I1=design_intercrop(dist_intra =dist_intra,dist_inter =dist_inter,designType =designType,dist_intercrop =dist_intercrop,orientation = orientation,pointSize =pointSize,lim = lim, I_dist_intra = I_dist_intra,I_dist_inter =I_dist_inter,I_designType = I_designType)
# #
# I2=design_intercrop(dist_intra =dist_intra,dist_inter =dist_inter,designType =designType,dist_intercrop =dist_intercrop,orientation = orientation,pointSize =pointSize,lim = lim, I_dist_intra = 2,I_dist_inter =1,I_designType = 'square3')
# 
# 
# ggplot()+
#   geom_point(data=I1$designPalm,aes(x=x,y=y,col= 'palms'),shape=8,size=pointSize)+
#   geom_point(data=I1$designI,aes(x=x,y=y,col='intercrop1'),size=pointSize-1,alpha=0.8)+
#   geom_point(data=I2$designI,aes(x=x,y=y,col='intercrop2'),size=pointSize-1,pch=17,alpha=0.8)+
#   xlim(c(0,lim))+
#   ylim(c(0,lim))


# test=plot_design(dist_intra = 2,dist_inter = 4,dist_intercrop = 6,designType = 'square4',orientation = 'NS',pointSize = 2,lim = 50)
# 
# test$plot+
#   ylim(c(-50,50))
