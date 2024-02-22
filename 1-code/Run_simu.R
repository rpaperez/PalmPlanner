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
source('./1-code/Mapping_light.R')


# inputs ------------------------------------------------------------------

MAP=180
d_inter=7.8
d_intra=9
d_intercrop=NULL
designType='quincunx'
path_designs='./2-outputs/Run_simu/planting_designs/'
pathVpalmParam='./2-outputs/Generate_VPalm_param/'
pathArchimed='./1-code/archimed-phys.jar'
pathVpalmJar='./1-code/vpalm_biomech.jar'
pathOpf='./2-outputs/Run_simu/ops/opf/'
pathOPS='./2-outputs/Run_simu/ops/'
opfStepExport=14
orientation='NS'

RunSimu(d_inter=d_inter,d_intra=d_intra,d_intercrop =d_intercrop,designType =designType,  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =T,orientation=orientation)

#### map
paramFileName=paste0('Mockup_seed1_MAP_',MAP)
Create_map(d_inter =d_inter ,d_intra = d_intra,designType = designType,d_intercrop =d_intercrop, path_designs = path_designs,paramFileName=paramFileName,orientation = orientation)
  
  