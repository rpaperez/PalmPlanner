
# Check Archimed rotation option ------------------------------------------

### compare a design NS with archimed rotation with a scene directly designed to be EW
# R.PEREZ March 2024



# inputs -------------------------------------------------------------
source('./1-code/Generate_designs.R')
source('./1-code/helpers.R')
source('./1-code/Mapping_light.R')
path_designs='2-outputs/Run_simu/planting_designs/'
MAP=90
paramFileName=paste0('DA1_Average_MAP_',MAP)
pathVpalmParam='./2-outputs/Generate_VPalm_param/'
pathArchimed='./1-code/archimed-phys.jar'
pathVpalmJar='./1-code/vpalm_biomech.jar'
pathOpf='./2-outputs/Run_simu/ops/opf/'
pathOPS='./2-outputs/Run_simu/ops/'
opfStepExport=6

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

## square

dist_intercrop=NULL
dist_intra=13
dist_inter=13


RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType ='square',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="NS",twist=0)

NSt0=Create_map(designType ='square' ,d_inter =13,d_intra = 13,d_intercrop = NULL,twist=0,path_designs =path_designs,paramFileName =  paramFileName,orientation ="NS" )

RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType ='square',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="NS",twist=60)

NSt60=Create_map(designType ='square' ,d_inter =13,d_intra = 13,d_intercrop = NULL,twist=60,path_designs =path_designs,paramFileName =  paramFileName,orientation ="NS" )


RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType ='square',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="NS",twist=90)

NSt90=Create_map(designType ='square' ,d_inter =13,d_intra = 13,d_intercrop = NULL,twist=90,path_designs =path_designs,paramFileName =  paramFileName,orientation ="NS" )

RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType ='square',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="EW",twist=0)

EWt0=Create_map(designType ='square' ,d_inter =13,d_intra = 13,d_intercrop = NULL,twist=0,path_designs =path_designs,paramFileName =  paramFileName,orientation ="EW" )

RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType ='square',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation="EW",twist=90)

EWt90=Create_map(designType ='square' ,d_inter =13,d_intra = 13,d_intercrop = NULL,twist=90,path_designs =path_designs,paramFileName =  paramFileName,orientation ="EW" )


NSt0$plot2
NSt60$plot2
EWt0$plot2
EWt90$plot2

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
