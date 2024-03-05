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


# designType='quincunx'

path_designs='./2-outputs/Run_simu/planting_designs/'
pathVpalmParam='./2-outputs/Generate_VPalm_param/'
pathArchimed='./1-code/archimed-phys.jar'
pathVpalmJar='./1-code/vpalm_biomech.jar'
pathOpf='./2-outputs/Run_simu/ops/opf/'
pathOPS='./2-outputs/Run_simu/ops/'
opfStepExport=6

replanting=T
paramFileName=paste0('DA1_Average_MAP_',MAP)


### test archimed rotation

generate_design(dist_intra = 9,dist_inter = 9,dist_intercrop =NULL ,designType ='square_bis' ,orientation = 'NS',pointSize = 3,replanting = T)$plot

### check design
# generate_design(dist_intra = dist_intra,dist_inter = dist_inter,dist_intercrop =dist_intercrop ,designType =designType ,orientation = orientation,pointSize = 3,replanting = T)$plot


generate_design(dist_intra = 9,dist_inter = 9,dist_intercrop =NULL ,designType ='square' ,orientation = 'NS',pointSize = 3,replanting = T)$plot
generate_design(dist_intra = 12,dist_inter =12,dist_intercrop =NULL ,designType ='square' ,orientation = 'NS',pointSize = 3,replanting = T)$plot

### run simu

## qunicunx
designType='quincunx'
dist_intercrop=NULL
for (orientation in c('NS','EW')){

  lh=list(c(7.8,9),c(9.5,11),c(15.6,18))
  for (i in lh){
   
  dist_intra=i[2]
  dist_inter=i[1]
  
  RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType =designType,  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation=orientation)
  }
}

## square
designType='square'
dist_intercrop=NULL
for (orientation in c('NS','EW')){
  
  lh=list(c(8,9),c(9,9),c(11,9),c(12,12))
  for (i in lh){
    
    dist_intra=i[2]
    dist_inter=i[1]
    
    RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType =designType,  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation=orientation)
  }
}



## quincunx2
designType='quincunx2'

for (orientation in c('NS','EW')){
  
  lh=list(c(7.8,9,15.6),c(7.8,9,12),c(6.8,9,16.6),c(6,9,17.4),c(6.9,8,16.5))
  for (i in lh){
    
    dist_intra=i[2]
    dist_inter=i[1]
    dist_intercrop=i[3]
    
    RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType =designType,  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation=orientation)
  }
}

## square2
designType='square2'

for (orientation in c('NS','EW')){
  
  lh=list(c(7.8,9,12),c(9,12,12))
  for (i in lh){
    
    dist_intra=i[2]
    dist_inter=i[1]
    dist_intercrop=i[3]
    
    RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType =designType,  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation=orientation)
  }
}

## quincunx3
designType='quincunx3'

for (orientation in c('NS','EW')){
  
  lh=list(c(7.8,9,15.6),c(6.8,9,17.6))
  for (i in lh){
    
    dist_intra=i[2]
    dist_inter=i[1]
    dist_intercrop=i[3]
    
    RunSimu(d_inter=dist_inter,d_intra=dist_intra,d_intercrop =dist_intercrop,designType =designType,  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation=orientation)
  }
}

## quincunx4
for (orientation in c('NS','EW')){
RunSimu(d_inter=7.8,d_intra=9,d_intercrop =15.6,designType ='quincunx4',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation=orientation)
}

## quincunx5
for (orientation in c('NS','EW')){
  RunSimu(d_inter=7.8,d_intra=9,d_intercrop =15.6,designType ='quincunx5',  MAP=MAP,pathVpalmParam=pathVpalmParam,pathArchimed=pathArchimed,path_designs=path_designs,pathVpalmJar=pathVpalmJar,pathOpf=pathOpf,pathOPS=pathOPS,run_photosynthesis=T,opfStepExport=opfStepExport,overwrite =F,orientation=orientation)
}


#### map


Create_map(d_inter =dist_inter ,d_intra = dist_intra,designType = designType,d_intercrop =dist_intercrop, path_designs = path_designs,paramFileName=paramFileName,orientation = orientation)
  
  