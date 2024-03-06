# Run simulation for ECOBREEDAF_SMARTRI ------------------------------------------
# R.PEREZ March 2024



# inputs -------------------------------------------------------------
source('./1-code/Generate_designs.R')
source('./1-code/helpers.R')
source('./1-code/Mapping_light.R')
path_designs='2-outputs/Run_simu/planting_designs/'
MAP=180
# paramFileName=paste0('DA1_Average_MAP_',MAP)
paramFileName=paste0('Mockup_seed1_MAP_',MAP)
pathVpalmParam='./2-outputs/Generate_VPalm_param/'
pathArchimed='./1-code/archimed-phys.jar'
pathVpalmJar='./1-code/vpalm_biomech.jar'
pathOpf='./2-outputs/Run_simu/ops/opf/'
pathOPS='./2-outputs/Run_simu/ops/'
opfStepExport=14
meteoFile='meteoPalembang.csv'

im=readPNG("north.png")


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

# run simulation ------------------------------------------------------------

### ref####

l=9.21
h=round(sqrt(3*(l**2)/4),2)
designType ='quincunx'
orientation='NS'

RunSimu(paramFileName=paramFileName,d_inter=h,d_intra=l,d_intercrop =NULL,designType =designType,  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="NS",twist=0,meteoFile =meteoFile )

Create_map(designType =designType ,d_inter =h,d_intra = l,d_intercrop = NULL,twist=0,path_designs =path_designs,paramFileName =  paramFileName,orientation =orientation)


design_name=paste0('quincunx_inter',h,'_intra',l,'_twist',0)

fileMap=paste0('2-outputs/Mapping_Light/',paramFileName,'_',design_name,'_',orientation,'.csv')

grid= data.table::fread(paste0(path_designs,design_name,'.csv'))
density=floor(nrow(grid1)*10000/(unique(grid$xmax)*unique(grid$ymax)))
grid_fin=data.table::fread(file =fileMap,dec = '.',sep = ',')


T0=grid_fin%>%
  mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
  ggplot(aes(x=x, y=y,fill=Intercepted_rel))+
  geom_tile()+
  geom_point(aes(x=x_tree_1,y=y_tree_1),pch=8,col='black',size=3)+
  geom_point(aes(x=x_tree_2,y=y_tree_2),pch=8,col='black',size=3)+
  ylim(low= 0, high= 50)+
  xlim(low= 0, high=50)+
  labs(y = 'Intra row distance (m)', x = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
  ggtitle(paste(density,' plants.ha-1'))+
  scale_fill_viridis(option = 'viridis',limits = c(0, 30))+
  # scale_fill_gradient2(low = 'forestgreen',mid = '#addd8e',high = '#f7fcb9',limits=c(0,100))+
  # scale_color_viridis(option = 'viridis',limits = c(0, 100))+
  coord_equal()+
  myTheme+
  theme(legend.position = 'bottom')


visu=ggdraw() +
  draw_plot(T0) +
  draw_image(im,x = 0.4,y = 0.4,scale=0.1)

G0=grid_fin%>%
  group_by(x)%>%
  summarize(Intercepted_rel=mean(Intercepted_rel),
            x_tree_1=mean(x_tree_1),
            x_tree_2=mean(x_tree_2))%>%
  ungroup()%>%
  ggplot(aes(x=x, y=Intercepted_rel,col=Intercepted_rel))+
  geom_line()+
  geom_vline(aes(xintercept = x_tree_1),col='forestgreen',lwd=1)+
  geom_vline(aes(xintercept = x_tree_2),col='forestgreen',lwd=1)+
  scale_color_viridis(option = 'viridis',limits = c(0, 30))+
  xlim(low= 0, high=min(c(max(grid_fin$y),max(grid_fin$x))))+
  ylim(c(0,30))+
  ggtitle(paste(density,' plants.ha-1'))+
  labs(x = 'x (m)', y =  expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)'))+
  myTheme+
  theme(legend.position = 'none')




### 1 line out of 2####
l=9.21
h=2*round(sqrt(3*(l**2)/4),2)
designType ='quincunx'
orientation='NS'

RunSimu(paramFileName=paramFileName,d_inter=h,d_intra=l,d_intercrop =NULL,designType ='quincunx',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="NS",twist=0,meteoFile =meteoFile )

# Create_map(designType =designType ,d_inter =h,d_intra = l,d_intercrop = NULL,twist=0,path_designs =path_designs,paramFileName =  paramFileName,orientation =orientation)


design_name=paste0('quincunx_inter',h,'_intra',l,'_twist',0)

fileMap=paste0('2-outputs/Mapping_Light/',paramFileName,'_',design_name,'_',orientation,'.csv')

grid1= data.table::fread(paste0(path_designs,design_name,'.csv'))
density1=floor(nrow(grid1)*10000/(unique(grid1$xmax)*unique(grid1$ymax)))
grid_fin1=data.table::fread(file =fileMap,dec = '.',sep = ',')


T1=grid_fin1%>%
  mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
  ggplot(aes(x=x, y=y,fill=Intercepted_rel))+
  geom_tile()+
  geom_point(aes(x=x_tree_1,y=y_tree_1),pch=8,col='black',size=3)+
  geom_point(aes(x=x_tree_2,y=y_tree_2),pch=8,col='black',size=3)+
  ylim(low= 0, high= 50)+
  xlim(low= 0, high=50)+
  labs(y = 'Intra row distance (m)', x = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
  ggtitle(paste(density1,' plants.ha-1'))+
  scale_fill_viridis(option = 'viridis',limits = c(0, 30))+
  # scale_fill_gradient2(low = 'forestgreen',mid = '#addd8e',high = '#f7fcb9',limits=c(0,100))+
  # scale_color_viridis(option = 'viridis',limits = c(0, 100))+
  coord_equal()+
  myTheme+
  theme(legend.position = 'bottom')


visu1=ggdraw() +
  draw_plot(T1) +
  draw_image(im,x = 0.4,y = 0.4,scale=0.1)

G1=grid_fin1%>%
  group_by(x)%>%
  summarize(Intercepted_rel=mean(Intercepted_rel),
            x_tree_1=mean(x_tree_1),
            x_tree_2=mean(x_tree_2))%>%
  ungroup()%>%
  ggplot(aes(x=x, y=Intercepted_rel,col=Intercepted_rel))+
  geom_line()+
  geom_vline(aes(xintercept = x_tree_1),col='forestgreen',lwd=1)+
  geom_vline(aes(xintercept = x_tree_2),col='forestgreen',lwd=1)+
  scale_color_viridis(option = 'viridis',limits = c(0, 30))+
  xlim(low= 0, high=min(c(max(grid_fin$y),max(grid_fin$x))))+
  ylim(c(0,30))+
  ggtitle(paste(density1,' plants.ha-1'))+
  labs(x = 'x (m)', y =  expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)'))+
  myTheme+
  theme(legend.position = 'none')

### 1 line out of 3####
l=9.21
h=round(sqrt(3*(l**2)/4),2)
d_intercrop=2*h
designType ='quincunx2'
design_name=paste0(designType,'_inter',h,'_intra',l,'_intercrop',d_intercrop,'_twist',0)



RunSimu(paramFileName=paramFileName,d_inter=h,d_intra=l,d_intercrop =d_intercrop,designType =designType,  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="NS",twist=0,meteoFile =meteoFile )

r=Create_map(designType =designType ,d_inter =h,d_intra = l,d_intercrop = d_intercrop,twist=0,path_designs =path_designs,paramFileName =  paramFileName,orientation =orientation)


fileMap=paste0('2-outputs/Mapping_Light/',paramFileName,'_',design_name,'_',orientation,'.csv')

grid2= data.table::fread(paste0(path_designs,design_name,'.csv'))
density2=floor(nrow(grid2)*10000/(unique(grid2$xmax)*unique(grid2$ymax)))
grid_fin2=data.table::fread(file =fileMap,dec = '.',sep = ',')


T2=grid_fin2%>%
  mutate(Intercepted_rel=ifelse(Intercepted_rel>100,100,Intercepted_rel))%>%
  ggplot(aes(x=x, y=y,fill=Intercepted_rel))+
  geom_tile()+
  geom_point(aes(x=x_tree_1,y=y_tree_1),pch=8,col='black',size=3)+
  geom_point(aes(x=x_tree_2,y=y_tree_2),pch=8,col='black',size=3)+
  ylim(low= 0, high= 50)+
  xlim(low= 0, high=50)+
  labs(y = 'Intra row distance (m)', x = 'Inter row distance (m)',fill= expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)')) +
  ggtitle(paste(density2,' plants.ha-1'))+
  scale_fill_viridis(option = 'viridis',limits = c(0, 30))+
  # scale_fill_gradient2(low = 'forestgreen',mid = '#addd8e',high = '#f7fcb9',limits=c(0,100))+
  # scale_color_viridis(option = 'viridis',limits = c(0, 100))+
  coord_equal()+
  myTheme+
  theme(legend.position = 'bottom')


visu2=ggdraw() +
  draw_plot(T2) +
  draw_image(im,x = 0.4,y = 0.4,scale=0.1)

G2=grid_fin2%>%
  group_by(x)%>%
  summarize(Intercepted_rel=mean(Intercepted_rel),
            x_tree_1=mean(x_tree_1),
            x_tree_2=mean(x_tree_2))%>%
  ungroup()%>%
  ggplot(aes(x=x, y=Intercepted_rel,col=Intercepted_rel))+
  geom_line()+
  geom_vline(aes(xintercept = x_tree_1),col='forestgreen',lwd=1)+
  geom_vline(aes(xintercept = x_tree_2),col='forestgreen',lwd=1)+
  scale_color_viridis(option = 'viridis',limits = c(0, 30))+
  xlim(low= 0, high=min(c(max(grid_fin$y),max(grid_fin$x))))+
  ylim(c(0,30))+
  ggtitle(paste(density2,' plants.ha-1'))+
  labs(x = 'x (m)', y =  expression ('Transmitted light (%)'),col=expression ('Transmitted light (%)'))+
  myTheme+
  theme(legend.position = 'none')

plot_grid(visu,visu2,visu1,nrow=1)

plot_grid(G0,G2,G1,nrow=1)
