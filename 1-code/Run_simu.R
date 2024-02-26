# Load packages -----------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("VEZY/Vpalmr")
packs <- c("lubridate", "stringr", 'tidyverse','viridis','Vpalmr','data.table','yaml')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

source('./1-code/helpers.R')
source('./1-code/Generate_designs.R')
source('./1-code/Mapping_light.R')


# inputs ------------------------------------------------------------------


MAP=90

l=18
h=15.6

dist_intra=l
dist_intercrop=NULL
dist_inter=h
designType='quincunx'

path_designs='./2-outputs/Run_simu/planting_designs/'
pathVpalmParam='./2-outputs/Generate_VPalm_param/'
pathArchimed='./1-code/archimed-phys.jar'
pathVpalmJar='./1-code/vpalm_biomech.jar'
pathOpf='./2-outputs/Run_simu/ops/opf/'
pathOPS='./2-outputs/Run_simu/ops/'
opfStepExport=6

replanting=T


### check design
generate_design(dist_intra = dist_intra,dist_inter = dist_inter,dist_intercrop =dist_intercrop ,designType =designType ,orientation = orientation,pointSize = 3,replanting = T)$plot

### run simu


## qunicunx
designType='quincunx'
for (orientation in c('NS','EW')){
  RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType =designType,  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =T,orientation=orientation)
}






#### map

paramFileName=paste0('DA1_Average_MAP_',MAP)
Create_map(d_inter =dist_inter ,d_intra = dist_intra,designType = designType,d_intercrop =dist_intercrop, path_designs = path_designs,paramFileName=paramFileName,orientation = orientation)
  
  