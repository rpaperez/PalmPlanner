
# Check Archimed rotation option ------------------------------------------

### compare a design NS with archimed rotation with a scene directly designed to be EW
# R.PEREZ March 2024



# inputs -------------------------------------------------------------
source('./1-code/Generate_designs.R')
source('./1-code/helpers.R')
source('./1-code/Mapping_light.R')


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

# import data -------------------------------------------------------------


### square design


path_designs='2-outputs/Run_simu/planting_designs/'
MAP=10
# paramFileName=paste0('DA1_Average_MAP_',MAP)
paramFileName='3000_100_MAP_10'
pathVpalmParam='./2-outputs/Generate_VPalm_param/'
pathArchimed='./1-code/archimed-phys.jar'
pathVpalmJar='./1-code/vpalm_biomech.jar'
pathOpf='./2-outputs/Run_simu/ops/opf/'
pathOPS='./2-outputs/Run_simu/ops/'
opfStepExport=6
dist_intercrop=NULL
dist_intra=3
dist_inter=3
twist=0
orientation="EW"
designType ='square'

RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType =designType,  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation=orientation,twist=twist,paramFileName =paramFileName,meteoFile='meteoPalembang.csv',plant_model = 'elaeis' )


design_name=paste0(designType,'_inter',dist_inter,'_intra',dist_intra,'_twist',twist)

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
                irradiation= Ra_PAR_q*area*step_duration,
                irradiance= Ra_PAR_q)

planFin=merge(plane_df_step,meteo%>%select(step_number,hour_start)) %>% 
  mutate(intercepted=Ra_PAR_q/area*10**-6)

planFin%>%
  # filter(step_number>10) %>%
  ggplot(aes(x=x, y=y,fill=intercepted))+
  geom_tile()+
  facet_wrap(~step_number)+
  ggtitle(paste(paramFileName,design_name,orientation))
# 
# NSt0=Create_map(designType ='square' ,d_inter =dist_intra,d_intra = dist_inter,d_intercrop = NULL,twist=0,path_designs =path_designs,paramFileName =  paramFileName,orientation ="NS" )
# 
# RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType ='square',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="NS",twist=60,paramFileName =paramFileName,meteoFile='meteoPalembang.csv')
# 
# NSt60=Create_map(designType ='square' ,d_inter =dist_inter,d_intra = dist_intra,d_intercrop = NULL,twist=60,path_designs =path_designs,paramFileName =  paramFileName,orientation ="NS" )
# 
# plot_grid(NSt0$plot2,NSt60$plot2)
# 

# 
# RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType ='square',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="NS",twist=90)
# 
# NSt90=Create_map(designType ='square' ,d_inter =13,d_intra = 13,d_intercrop = NULL,twist=90,path_designs =path_designs,paramFileName =  paramFileName,orientation ="NS" )
# 
# RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType ='square',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="EW",twist=0)
# 
# EWt0=Create_map(designType ='square' ,d_inter =13,d_intra = 13,d_intercrop = NULL,twist=0,path_designs =path_designs,paramFileName =  paramFileName,orientation ="EW" )
# 
# RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType ='square',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="EW",twist=90)
# 
# EWt90=Create_map(designType ='square' ,d_inter =13,d_intra = 13,d_intercrop = NULL,twist=90,path_designs =path_designs,paramFileName =  paramFileName,orientation ="EW" )
# 
# 
# NSt0$plot2
# NSt60$plot2
# EWt0$plot2
# EWt90$plot2

# ROTATION IS NOT WORKING!!!!!!!!! ----------------------------------------



#
# cowplot::plot_grid(NSt0$plot2,NSt90$plot2,EWt0$plot2,EWt90$plot2,ncol=4)
# 



# NS=Create_map(designType ='square' ,d_inter =12,d_intra = 12,d_intercrop = NULL,path_designs =path_designs,paramFileName =  paramFileName,orientation ="NS" )
# 
# RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType ='square',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="EW")
# 
# EW_rot=Create_map(designType ='square' ,d_inter =12,d_intra = 12,d_intercrop = NULL,path_designs =path_designs,paramFileName =  paramFileName,orientation ="EW" )
# # 
# 
# 
# RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType ='square_bis',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="NS")
# 
# EW_noRot=Create_map(designType ='square_bis' ,d_inter =12,d_intra = 12,d_intercrop = NULL,path_designs =path_designs,paramFileName =  paramFileName,orientation ="NS" )
# # 
# 
# # cowplot::plot_grid(NS$plot,EW_rot$plot)
# cowplot::plot_grid(NS$plot2,EW_rot$plot2,EW_noRot$plot2,ncol=3)
# 
# 
# ### quincunx design
# l=18
# h=round(sqrt(3*(l**2)/4),1)
# 
# NSq=Create_map(designType ='quincunx' ,d_inter =h,d_intra = l,d_intercrop = NULL,path_designs =path_designs,paramFileName =  paramFileName,orientation ="NS" )
# 
# EWq_rot=Create_map(designType ='quincunx' ,d_inter =h,d_intra = l,d_intercrop = NULL,path_designs =path_designs,paramFileName =  paramFileName,orientation ="EW" )
# 
# RunSimu(d_inter=h,d_intra=l,d_intercrop =NULL,designType ='quincunx_bis',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation='NS')
# 
# 
# EWq_noRot=Create_map(designType ='quincunx_bis' ,d_inter =h,d_intra = l,d_intercrop = NULL,path_designs =path_designs,paramFileName =  paramFileName,orientation ="NS" )
# 
# cowplot::plot_grid(NSq$plot2,EWq_rot$plot2,EWq_noRot$plot2,ncol=3)
# cowplot::plot_grid(NSq$plot,EWq_rot$plot,EWq_noRot$plot,ncol=3)
