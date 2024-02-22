
packs <- c("lubridate", "stringr", 'tidyverse','viridis','Vpalmr','data.table')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)


source('1-code/helpers.R')

### generate Vpalm param
MAP=36
e=5
s=1

Generate_Vpalm_param(MAP_requested =MAP,elastic_modulus =e,shear_modulus =s ,filename =paste(e,s,sep = '_') )

###generate

pathVpalmJar='./1-code/vpalm_biomech.jar'
pathOpf='./2-outputs/testingOpf/opf/'
pathVpalmParam="./2-outputs/Generate_VPalm_param/"
paramFileName=paste(e,s,'MAP',MAP,sep = '_')
system(command = paste0('java -jar ',pathVpalmJar,' ',pathVpalmParam,paramFileName,'.txt',' ',pathOpf,paramFileName,'.opf'))
