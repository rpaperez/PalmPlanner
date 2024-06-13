#### Visualisation of archimed outputs###
##R Perez 12/06/2023

# Load packages -----------------------------------------------------------

packs <- c("lubridate", "stringr", "ggplot2",'dplyr','ggpmisc','plotly','archimedR','viridis','ggrepel','cowplot','png')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack))}
lapply(packs, InstIfNec)

# install archimedR
if (require('archimedR')==F){
  install.packages('remotes')
  remotes::install_github('VEZY/archimedR')
}

#' Create_map
#'
#' @param d_inter distance between rows of palm trees
#' @param d_intra distance within rows of palm trees
#' @param path_designs path to save planting designs
#'@param d_intercrop: distance for intercrop
#' @param designType type of design 
#' @param paramFileName name of the Vpalm param file (.txt)
#' @param orientation orientation of the scene (NS: North-South or EW: East-West)
#' @param lim limit of the area for plotting map (m)
#' @param twist twist/rotation of the palm stem in the ops file (in degree)

#' @return map of transmitted light to the ground
#' @export csv file of the map
#'
#' @examples
Create_map=function(designType=designType,d_inter=d_inter,d_intra=d_intra,d_intercrop=d_intercrop,path_designs=path_designs,paramFileName=paramFileName,orientation=orientation,lim=50){
  
  # d_inter=15.6
  # d_intra=18
  # d_intercrop=NULL
  # path_designs='2-outputs/Run_simu/planting_designs/'
  # paramFileName='Mockup_seed1_MAP_180'
  # orientation='NS'
  # lim=50
  # designType='square'
  
  # design_name=paste0('inter',d_inter,'-intra',d_intra)
  
  if (designType %in% c('square','quincunx')){
    design_name=paste0(designType,'_inter',d_inter,'_intra',d_intra)
    
  }
  if (!(designType %in% c('square','quincunx'))){
    design_name=paste0(designType,'_inter',d_inter,'_intra',d_intra,'_intercrop',d_intercrop)
  }
  
  
  ### checking for existing map
  existing_files=list.files(path ='2-outputs/Mapping_Light',pattern = '.csv',full.names = T)
  fileMap=paste0('2-outputs/Mapping_Light/',paramFileName,'_',design_name,'_',orientation,'.csv')
  
  if (!(fileMap %in% existing_files)){
    
    print(paste("generating the file:",fileMap))
    
    ###archimed outputs

    
    sim_folder=paste0('2-outputs/Run_simu/output/',design_name,'_',paramFileName,'_',orientation,'/')
    
    files_sim= 
      sim_folder%>%
      list.files(recursive = T, full.names = TRUE)
    
    
    
    ###meteo
    path_meteo= files_sim[grep("meteo.csv",files_sim)]
    
    # Importing the meteorology file from the first simulation (all are the same):
    meteo= archimedR::import_meteo(x = file.path(path_meteo[1]))%>%
      mutate(date= as.POSIXct(date)+ seconds(hour_start))
    
    
    ###conversion factor W.m-2.30mn of GR-->MJ.m-2.day-1 of PAR
    # conMJday=0.48*1800*10**-6
    
    ###conversion factor W.m-2.60mn of GR-->MJ.m-2.day-1 of PAR
    conMJday=0.48*3600*10**-6
    
    ##"incident PAR in MJ.m-2.day-1
    PARinc=as.numeric(meteo%>%
                        summarize(PARinc=sum(`RI_TIR_f`*conMJday)))
    
    
    
    ###import summary outputs
    path_sum= files_sim[grep("summary.csv",files_sim)]
    
    # Importing the meteorology file from the first simulation (all are the same):
    summ=data.table::fread(file =  (path_sum[1]))
    
    stepDuration=unique(summ$`step_duration`)
    
    # Importing the node values (main output):
    path_nodes= files_sim[grep("component_values.csv",files_sim)]
    
    nodes= 
      lapply(path_nodes, function(x){
        name= 
          x%>%dirname()%>%strsplit(split = "/")%>%unlist()%>%
          tail(1)
        data.table::fread(x)%>%mutate(Design= name)
      })%>%data.table::rbindlist()%>%tibble::as_tibble()
    
    ngridT= 
      nodes%>%
      ungroup()%>%
      filter(type=="Cobblestone")%>%
      filter(step_number==step_number[1])%>%
      group_by(Design)%>%
      summarise(ngrid= n(), area_grid= sum(.data$area))%>%
      ungroup()%>%
      mutate(Design=design_name)%>%
      data.frame()
    
    
    
    # Grid index: 
    grid_dim= data.table::fread(paste0(path_designs,design_name,'.csv'))%>%
      mutate(Design=design_name,
             ngrid=ngridT$ngrid,
             area_grid=ngridT$area_grid,
             surf_grid= area_grid/ngrid,
             x_length= xmax-xmin,
             y_length= ymax-ymin,
             grid_n_x= round(x_length/sqrt(surf_grid)),
             grid_n_y= round(y_length/sqrt(surf_grid))
      )%>%
      select(Design,  xmin ,xmax,ymin,ymax,ngrid,area_grid,Design,surf_grid,x_length,y_length ,grid_n_x,grid_n_y)%>%
      distinct()

      
    grid_index= expand.grid(y= (1:grid_dim$grid_n_y)*sqrt(grid_dim$surf_grid),
                            x= (1:grid_dim$grid_n_x)*sqrt(grid_dim$surf_grid))%>%
      mutate(id= 1:nrow(.))%>%
      mutate(Design=grid_dim$Design)
    
    des_=  data.table::fread(paste0(path_designs,design_name,'.csv'))%>%select(x,y)%>%unlist(.)
    trees_positions= des_%>%matrix(nrow = 1)%>%as.data.frame()%>%setNames(names(des_))%>%
      mutate(Design= design_name)
    
    
    x=trees_positions$Design
    found= grep(x,grid_dim$Design)

    if(found==1 & !('x1' %in% colnames(trees_positions))){
      trees_positions=trees_positions%>%
        rename(x1=x,y1=y)
    }
    if(found>1){
    trees_positions= trees_positions[trees_positions$Design==x,]%>%
      .[rep(1,length(found)),]%>%
      mutate(Design= grid_dim$Design[grep(x,grid_dim$Design)])
    }
    
    ## get 5 trees positions to be generic
    nbT=(length(trees_positions)-1)/2
    trees_positions=trees_positions%>%
      mutate(x2=ifelse(nbT>1,x2,NA),
            x3=ifelse(nbT>2,x3,NA),
             x4=ifelse(nbT>3,x4,NA),
             x5=ifelse(nbT>4,x5,NA),
            y2=ifelse(nbT>1,y2,NA),
             y3=ifelse(nbT>2,y3,NA),
             y4=ifelse(nbT>3,y4,NA),
             y5=ifelse(nbT>4,y5,NA))
    
    
    grid_dist= 
      merge(grid_index,trees_positions,by = "Design",all.x=F)%>%
      mutate(dist_tree_1= sqrt((x - x1)^2 + (y - y1)^2),
             dist_tree_2= sqrt((x - x2)^2 + (y - y2)^2),
             dist_tree_3= sqrt((x - x3)^2 + (y - y3)^2),
             dist_tree_4= sqrt((x - x4)^2 + (y - y4)^2),
             dist_tree_5= sqrt((x - x5)^2 + (y - y5)^2),
             dist_tree_x1= abs(x - x1),
             dist_tree_x2= abs(x - x2),
             dist_tree_x3= abs(x - x3),
             dist_tree_x4= abs(x - x4),
             dist_tree_x5= abs(x - x5),
             x_tree_1= x1, x_tree_2= x2,
             x_tree_3= x3, x_tree_4= x4,
             x_tree_5= x5,
             y_tree_1=y1,y_tree_2=y2,
             y_tree_3=y3,y_tree_4=y4,y_tree_5=y5)%>%
      mutate(dist_tree= pmin(dist_tree_1,dist_tree_2,dist_tree_3,dist_tree_4,dist_tree_5,na.rm = T),
             dist_tree_x= pmin(dist_tree_x1,dist_tree_x2,dist_tree_x3,dist_tree_x4,dist_tree_5,na.rm = T))
    
    
    plane_df= 
      nodes%>%
      filter(type=="Cobblestone")%>%
      mutate(Design=design_name)%>%
      mutate(component_id= component_id-1)%>% # id 1 was the scene
      # dplyr::left_join(Area_plots, by= "Design")%>%
      dplyr::left_join(meteo%>%select(date,step), by= c("step_number"= "step"))%>%
      dplyr::left_join(grid_dist, by= c("Design","component_id"= "id"))
    
    
    plane_df$Design= as.factor(plane_df$Design)
    
    plane_df=plane_df%>%
      filter(Design==unique(grid_dist$Design))
    
    
    plane_df_step= 
      plane_df%>%
      dplyr::mutate(Date= date,
                    irradiation= Ri_PAR_q*area*step_duration,
                    irradiance= Ri_PAR_q)
    
    grid_df_day= 
      plane_df_step%>%
      filter(Design %in% unique(plane_df_step$Design))%>%
      group_by(Design,component_id)%>%
      summarise(Date= mean(.data$date),
                Intercepted= sum(Ri_PAR_q/area*10**-6),
                # absEnergy_withScattering_PAR J grid-1
                # Global intercepted radiation in J grid-1 d-1
                # Area_plot= mean(.data$Area_plot),
                dist_tree= mean(dist_tree),
                dist_tree_x= mean(dist_tree_x),
                # density= unique(density),
                x= unique(x),
                y=unique(y),
                x_tree_1= unique(x_tree_1), 
                x_tree_2= unique(x_tree_2),
                x_tree_3= unique(x_tree_3), 
                x_tree_4= unique(x_tree_4),
                x_tree_5= unique(x_tree_5),
                y_tree_1= unique(y_tree_1), 
                y_tree_2= unique(y_tree_2),
                y_tree_3= unique(y_tree_3), 
                y_tree_4= unique(y_tree_4),
                y_tree_5= unique(y_tree_5))%>%
      ungroup()
    
    reps=NULL
    
    if (designType %in% c('square','quincunx')){
      d_intercrop=0}
  
    
    repRows=ceiling(lim/(d_inter+d_intercrop))
    repCol=ceiling(lim/d_intra)
    
    for (r_x in 0:repRows){
      for (r_y in 0:repCol){
        
        # print(paste('r_x:',r_x,' r_y:',r_y))
        
        subRep=grid_df_day%>%
          group_by(Design)%>%
          mutate(xmax=max(x,na.rm=T),
                 ymax=max(y,na.rm=T),
                 x=x+r_x*xmax,
                 y=y+r_y*ymax,
                 x_tree_1=x_tree_1+r_x*xmax,
                 x_tree_2=x_tree_2+r_x*xmax,
                 x_tree_3=x_tree_3+r_x*xmax,
                 x_tree_4=x_tree_4+r_x*xmax,
                 x_tree_5=x_tree_5+r_x*xmax,
                 y_tree_1=y_tree_1+r_y*ymax,
                 y_tree_2=y_tree_2+r_y*ymax,
                 y_tree_3=y_tree_3+r_y*ymax,
                 y_tree_4=y_tree_4+r_y*ymax,
                 y_tree_5=y_tree_5+r_y*ymax
                 )%>%
          ungroup()%>%
          select(colnames(grid_df_day))
        
        reps=rbind(reps,subRep)
      }
    }
    

    grid_fin=reps%>%
      filter(x<=lim & y<=lim)%>%
      # filter(x_tree_1<=lim & y_tree_1<=lim)%>%
      # filter(x_tree_2<=lim & y_tree_2<=lim)%>%
      mutate(Intercepted_rel=Intercepted/PARinc*100)

    ### save the map
    data.table::fwrite(x = grid_fin,file =fileMap)
  }
  
  if (fileMap %in% existing_files){
    print(paste('loading existing file',fileMap))
    grid_fin=data.table::fread(file =fileMap,dec = '.',sep = ',')
  }
  
  ######## Plot 1####
  grid= data.table::fread(paste0(path_designs,design_name,'.csv'))
  density=floor(nrow(grid)*10000/(unique(grid$xmax)*unique(grid$ymax)))

  if(orientation=='NS'){
    if (designType %in% c('square','square_bis')){
      plot=grid_fin%>%
        mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
        ggplot(aes(x=x, y=y,fill=Intercepted_rel))+
        geom_tile()+
        geom_point(aes(x=x_tree_1,y=y_tree_1),pch=8,col='forestgreen',size=3)+
        ylim(low= 0, high= lim)+
        xlim(low= 0, high=lim)+
        labs(y = 'Intra row distance (m)', x = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
        ggtitle(paste(density,' plants.ha-1'))+
        scale_fill_viridis(option = 'viridis',limits = c(0, 100))+
        # scale_fill_gradient2(low = 'forestgreen',mid = '#addd8e',high = '#f7fcb9',limits=c(0,100))+
        # scale_color_viridis(option = 'viridis',limits = c(0, 100))+
        coord_equal()+
        myTheme+
        theme(legend.position = 'bottom')
    }
    if (designType %in% c('quincunx','quincunx_bis','quincunx2','square2')){
      plot=grid_fin%>%
        mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
        ggplot(aes(x=x, y=y,fill=Intercepted_rel))+
        geom_tile()+
        geom_point(aes(x=x_tree_1,y=y_tree_1),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=x_tree_2,y=y_tree_2),pch=8,col='forestgreen',size=3)+
        ylim(low= 0, high= lim)+
        xlim(low= 0, high=lim)+
        labs(y = 'Intra row distance (m)', x = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
        ggtitle(paste(density,' plants.ha-1'))+
        scale_fill_viridis(option = 'viridis',limits = c(0, 100))+
        # scale_fill_gradient2(low = 'forestgreen',mid = '#addd8e',high = '#f7fcb9',limits=c(0,100))+
        # scale_color_viridis(option = 'viridis',limits = c(0, 100))+
        coord_equal()+
        myTheme+
        theme(legend.position = 'bottom')
    }
    if (designType %in% c('quincunx3')){
      plot=grid_fin%>%
        mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
        ggplot(aes(x=x, y=y,fill=Intercepted_rel))+
        geom_tile()+
        geom_point(aes(x=x_tree_1,y=y_tree_1),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=x_tree_2,y=y_tree_2),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=x_tree_3,y=y_tree_3),pch=8,col='forestgreen',size=3)+
        ylim(low= 0, high= lim)+
        xlim(low= 0, high=lim)+
        ggtitle(paste(density,' plants.ha-1'))+
        labs(y = 'Intra row distance (m)', x = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
        scale_fill_viridis(option = 'viridis',limits = c(0, 100))+
        coord_equal()+
        myTheme+
        theme(legend.position = 'bottom')
    }
    if (designType %in% c('quincunx4')){
      plot=grid_fin%>%
        mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
        ggplot(aes(x=x, y=y,fill=Intercepted_rel))+
        geom_tile()+
        geom_point(aes(x=x_tree_1,y=y_tree_1),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=x_tree_2,y=y_tree_2),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=x_tree_3,y=y_tree_3),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=x_tree_4,y=y_tree_4),pch=8,col='forestgreen',size=3)+
        ylim(low= 0, high= lim)+
        xlim(low= 0, high=lim)+
        ggtitle(paste(density,' plants.ha-1'))+
        labs(y = 'Intra row distance (m)', x = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
        scale_fill_viridis(option = 'viridis',limits = c(0, 100))+
        coord_equal()+
        myTheme+
        theme(legend.position = 'bottom')
    }
    if (designType %in% c('quincunx5')){
      plot=grid_fin%>%
        mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
        ggplot(aes(x=x, y=y,fill=Intercepted_rel))+
        geom_tile()+
        geom_point(aes(x=x_tree_1,y=y_tree_1),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=x_tree_2,y=y_tree_2),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=x_tree_3,y=y_tree_3),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=x_tree_4,y=y_tree_4),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=x_tree_5,y=y_tree_5),pch=8,col='forestgreen',size=3)+
        ylim(low= 0, high= lim)+
        xlim(low= 0, high=lim)+
        ggtitle(paste(density,' plants.ha-1'))+
        labs(y = 'Intra row distance (m)', x = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
        scale_fill_viridis(option = 'viridis',limits = c(0, 100))+
        coord_equal()+
        myTheme+
        theme(legend.position = 'bottom')
    }
  }
  
  if(orientation=='EW'){
    if (designType =='square'){
      plot=grid_fin%>%
        mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
        ggplot(aes(x=y, y=x,fill=Intercepted_rel))+
        geom_tile()+
        geom_point(aes(x=y_tree_1,y=x_tree_1),pch=8,col='forestgreen',size=3)+
        ylim(low= 0, high= lim)+
        xlim(low= 0, high=lim)+
        ggtitle(paste(density,' plants.ha-1'))+
        labs(x = 'Intra row distance (m)', y = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
        scale_fill_viridis(option = 'viridis',limits = c(0, 100))+
        coord_equal()+
        myTheme+
        theme(legend.position = 'bottom')
    }
    if (designType %in% c('quincunx','quincunx2','square2')){
    plot=grid_fin%>%
      mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
      ggplot(aes(x=y, y=x,fill=Intercepted_rel))+
      geom_tile()+
      geom_point(aes(x=y_tree_1,y=x_tree_1),pch=8,col='forestgreen',size=3)+
      geom_point(aes(x=y_tree_2,y=x_tree_2),pch=8,col='forestgreen',size=3)+
      ylim(low= 0, high= lim)+
      xlim(low= 0, high=lim)+
      ggtitle(paste(density,' plants.ha-1'))+
      labs(x = 'Intra row distance (m)', y = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
      scale_fill_viridis(option = 'viridis',limits = c(0, 100))+
      coord_equal()+
      myTheme+
      theme(legend.position = 'bottom')
    }
    if (designType %in% c('quincunx3','square3')){
      plot=grid_fin%>%
        mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
        ggplot(aes(x=y, y=x,fill=Intercepted_rel))+
        geom_tile()+
        geom_point(aes(x=y_tree_1,y=x_tree_1),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=y_tree_2,y=x_tree_2),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=y_tree_3,y=x_tree_3),pch=8,col='forestgreen',size=3)+
        ylim(low= 0, high= lim)+
        xlim(low= 0, high=lim)+
        ggtitle(paste(density,' plants.ha-1'))+
        labs(x = 'Intra row distance (m)', y = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
        scale_fill_viridis(option = 'viridis',limits = c(0, 100))+
        coord_equal()+
        myTheme+
        theme(legend.position = 'bottom')
    }
    if (designType %in% c('quincunx4','square4')){
      plot=grid_fin%>%
        mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
        ggplot(aes(x=y, y=x,fill=Intercepted_rel))+
        geom_tile()+
        geom_point(aes(x=y_tree_1,y=x_tree_1),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=y_tree_2,y=x_tree_2),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=y_tree_3,y=x_tree_3),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=y_tree_4,y=x_tree_4),pch=8,col='forestgreen',size=3)+
        ylim(low= 0, high= lim)+
        xlim(low= 0, high=lim)+
        ggtitle(paste(density,' plants.ha-1'))+
        labs(x = 'Intra row distance (m)', y = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
        scale_fill_viridis(option = 'viridis',limits = c(0, 100))+
        coord_equal()+
        myTheme+
        theme(legend.position = 'bottom')
    }
    if (designType %in% c('quincunx5','square5')){
      plot=grid_fin%>%
        mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
        ggplot(aes(x=y, y=x,fill=Intercepted_rel))+
        geom_tile()+
        geom_point(aes(x=y_tree_1,y=x_tree_1),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=y_tree_2,y=x_tree_2),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=y_tree_3,y=x_tree_3),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=y_tree_4,y=x_tree_4),pch=8,col='forestgreen',size=3)+
        geom_point(aes(x=y_tree_5,y=x_tree_5),pch=8,col='forestgreen',size=3)+
        ylim(low= 0, high= lim)+
        xlim(low= 0, high=lim)+
        ggtitle(paste(density,' plants.ha-1'))+
        labs(x = 'Intra row distance (m)', y = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
        scale_fill_viridis(option = 'viridis',limits = c(0, 100))+
        coord_equal()+
        myTheme+
        theme(legend.position = 'bottom')
    }
    
  }
  
##### Plot 2#####
  
  if (designType  %in% c('square')){
    plot2=grid_fin%>%
      group_by(x)%>%
      summarize(Intercepted_rel=mean(Intercepted_rel),
                x_tree_1=mean(x_tree_1))%>%
      ungroup()%>%
      ggplot(aes(x=x, y=Intercepted_rel,col=Intercepted_rel))+
      geom_line()+
      geom_vline(aes(xintercept = x_tree_1),col='forestgreen',lwd=1)+
      scale_color_viridis(option = 'viridis',limits = c(0, 100))+
      xlim(low= 0, high=lim)+
      ylim(c(0,lim))+
      ggtitle(paste(density,' plants.ha-1'))+
      labs(x = 'x (m)', y =  expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)'))+
      myTheme+
      theme(legend.position = 'none')
    
  }
  
  
  
  if (designType %in% c('quincunx','quincunx2','square2')){
  plot2=grid_fin%>%
    group_by(x)%>%
    summarize(Intercepted_rel=mean(Intercepted_rel),
              x_tree_1=mean(x_tree_1),
              x_tree_2=mean(x_tree_2))%>%
    ungroup()%>%
    ggplot(aes(x=x, y=Intercepted_rel,col=Intercepted_rel))+
    geom_line()+
    geom_vline(aes(xintercept = x_tree_1),col='forestgreen',lwd=1)+
    geom_vline(aes(xintercept = x_tree_2),col='forestgreen',lwd=1)+
    scale_color_viridis(option = 'viridis',limits = c(0, 100))+
    xlim(low= 0, high=min(c(max(grid_fin$y),max(grid_fin$x))))+
    ylim(c(0,lim))+
    ggtitle(paste(density,' plants.ha-1'))+
  labs(x = 'x (m)', y =  expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)'))+
    myTheme+
  theme(legend.position = 'none')
  
  }
  
 
  
  if (designType %in% c('quincunx3','square3')){
    plot2=grid_fin%>%
      group_by(x)%>%
      summarize(Intercepted_rel=mean(Intercepted_rel),
                x_tree_1=mean(x_tree_1),
                x_tree_2=mean(x_tree_2),
                x_tree_3=mean(x_tree_3))%>%
      ungroup()%>%
      ggplot(aes(x=x, y=Intercepted_rel,col=Intercepted_rel))+
      geom_line()+
      geom_vline(aes(xintercept = x_tree_1),col='forestgreen',lwd=1)+
      geom_vline(aes(xintercept = x_tree_2),col='forestgreen',lwd=1)+
      geom_vline(aes(xintercept = x_tree_3),col='forestgreen',lwd=1)+
      scale_color_viridis(option = 'viridis',limits = c(0, 100))+
      xlim(low= 0, high=lim)+
      ylim(c(0,lim))+
      ggtitle(paste(density,' plants.ha-1'))+
      labs(x = 'x (m)', y =  expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)'))+
      myTheme+
      theme(legend.position = 'none')
  }
  if (designType %in% c('quincunx4','square4')){
    plot2=grid_fin%>%
      group_by(x)%>%
      summarize(Intercepted_rel=mean(Intercepted_rel),
                x_tree_1=mean(x_tree_1),
                x_tree_2=mean(x_tree_2),
                x_tree_3=mean(x_tree_3),
                x_tree_4=mean(x_tree_4))%>%
      ungroup()%>%
      ggplot(aes(x=x, y=Intercepted_rel,col=Intercepted_rel))+
      geom_line()+
      geom_vline(aes(xintercept = x_tree_1),col='forestgreen',lwd=1)+
      geom_vline(aes(xintercept = x_tree_2),col='forestgreen',lwd=1)+
      geom_vline(aes(xintercept = x_tree_3),col='forestgreen',lwd=1)+
      geom_vline(aes(xintercept = x_tree_4),col='forestgreen',lwd=1)+
      scale_color_viridis(option = 'viridis',limits = c(0, 100))+
      xlim(low= 0, high=lim)+
      ylim(c(0,lim))+
      ggtitle(paste(density,' plants.ha-1'))+
      labs(x = 'x (m)', y =  expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)'))+
      myTheme+
      theme(legend.position = 'none')
  }
  if (designType %in% c('quincunx5','square5')){
    plot2=grid_fin%>%
      group_by(x)%>%
      summarize(Intercepted_rel=mean(Intercepted_rel),
                x_tree_1=mean(x_tree_1),
                x_tree_2=mean(x_tree_2),
                x_tree_3=mean(x_tree_3),
                x_tree_4=mean(x_tree_4),
                x_tree_5=mean(x_tree_5))%>%
      ungroup()%>%
      ggplot(aes(x=x, y=Intercepted_rel,col=Intercepted_rel))+
      geom_line()+
      geom_vline(aes(xintercept = x_tree_1),col='forestgreen',lwd=1)+
      geom_vline(aes(xintercept = x_tree_2),col='forestgreen',lwd=1)+
      geom_vline(aes(xintercept = x_tree_3),col='forestgreen',lwd=1)+
      geom_vline(aes(xintercept = x_tree_4),col='forestgreen',lwd=1)+
      geom_vline(aes(xintercept = x_tree_5),col='forestgreen',lwd=1)+
      scale_color_viridis(option = 'viridis',limits = c(0, 100))+
      xlim(low= 0, high=lim)+
      ylim(c(0,lim))+
      ggtitle(paste(density,' plants.ha-1'))+
      labs(x = 'x (m)', y =  expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)'))+
      myTheme+
      theme(legend.position = 'none')
  }
  
  im=readPNG("north.png")
  
  visu=ggdraw() +
    draw_plot(plot) +
    draw_image(im,x = 0.4,y = 0.4,scale=0.1)


  list(plot=visu,plot2=plot2)
}
