##----------------------------------------------##
# Script to generate VPALM parameters ##
#----------------------------------------------##

# Load packages -----------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("VEZY/Vpalmr")


packs <- c("lubridate", "stringr", 'tidyverse','viridis','Vpalmr','data.table')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)


source('1-code/helpers.R')
source('1-code/VegetativeGrowth.R')

Generate_Vpalm_param=function(MAP_requested=MAP_requested,elastic_modulus=3026,shear_modulus=125,nbLeaves=45){

  list_param=readRDS('0-data/vpalm_template.rds')
  
  ###inputs
  coefPhylo=2.16
  nbLeafEmitted=round(MAP_requested*coefPhylo)
  nbLeaves=nbLeaves
  
  # elastic_modulus=3026
  # shear_modulus =125
  
  leafLengthSlope=0.05707789
  leafLengthIntercept=353.9938
  
  
  ranks=seq(1,nbLeaves,1)
  
  seed=1
  
  ### stem height
  
  
  stem_height=stem_height(X = MAP_requested,slope1= coef(modStem)['slope1'],slope2=coef(modStem)['slope2'],breakMAP=breakMAP)
  
  # stem height in Vpalm (y0+exp(coefStem*nbLeafEmitted))
  y0= 5
  list_param$residStemHeight=0
  list_param$coefStemHeight=log(stem_height-y0)/nbLeafEmitted
  
  ## rachis length
  seqNbL=seq(nbLeafEmitted-nbLeaves+1,nbLeafEmitted,1)
  seq_rac=data.frame(nbLeafEmitted=seqNbL,rank=rev(c(1:45)))%>%
    mutate(racL=sigmo(x=nbLeafEmitted,max=coef(modRac)['max'],slp=coef(modRac)['slp'],infl=coef(modRac)['infl']))
  
  modRachisL=nls(data = seq_rac,
                 formula =
                   racL ~ a*rank+b,
                 start= c(a=1,b=0),trace = F)
  
  rachisLengthIntercept=coef(modRachisL)['b']
  rachisLengthSlope=coef(modRachisL)['a']
  
  seq_rac=seq_rac%>%
    mutate(racL_sim=predict(modRachisL))

  # ggplot()+
  #   geom_point(data=seq_rac,aes(x=rank,y=racL))+
  #   geom_line(data=seq_rac,aes(x=rank,y=racL_sim,col='sim'))
  
  set.seed(seed = seed)
  
  rachisLenght_sim0=rachisLengthIntercept+ranks*rachisLengthSlope
  
  rachisLenght_sim=rachisLenght_sim0+rnorm(n =length(rachisLenght_sim0),mean = 0,sd = list_param$rachisLength_SDP)
  
  dif_racL=mean(rachisLenght_sim)-mean(seq_rac$racL)
  print(paste('Average error between observed and simualted rachis lenght:',dif_racL,'cm'))
  
  rachisFW_sim=(rachisLenght_sim-leafLengthIntercept)/leafLengthSlope
  
  list_param$rachisBiomass=round(rachisFW_sim)
  
  ### set params
  list_param$nbFronds_M=nbLeaves
  list_param$MAP_requested=MAP_requested
  list_param$nbLeafEmitted=nbLeafEmitted
  list_param$rachisTwistIniAngle_M = 4
  list_param$rachisTwistIniAngle_SDP = 2
  
  list_param$leafLengthSlope=leafLengthSlope
  
  list_param$leafLengthIntercept=leafLengthIntercept
  
  list_param$elastic_modulus=elastic_modulus
  list_param$shear_modulus =shear_modulus
  
  list_param$seed=seed
  
  
  VPalm_param=new_format_tree(data = list_param)
  
  filename=paste0('Mockup_seed',seed)
  
  test_params = Vpalmr::write_tree(data = VPalm_param, path = "2-outputs/Generate_VPalm_param/",
                                   name= filename, verbose = F, overwrite = TRUE)
  
}



