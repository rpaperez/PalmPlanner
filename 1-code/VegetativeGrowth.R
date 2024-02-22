##----------------------------------------------##
# Script to estimate vegetative growth with plant age
#----------------------------------------------##

# Load packages -----------------------------------------------------------

packs <- c("lubridate", "stringr", 'tidyverse','viridis','data.table')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)


# load data ---------------------------------------------------------------

don_raw=data.table::fread('0-data/VegetativeGrowth.csv')

don=don_raw%>%
  tidyr::gather(key = 'MAP',value ='Measurement', contains('MAP'))%>%
  mutate(MAP=as.numeric(str_remove(MAP,'MAP_')))


# var='StemHeight'
# var='FrondLength'
# # var='CollarGirth'
# don%>%
#   filter(Variable==var)%>%
#   ggplot(aes(x=MAP,y=Measurement))+
#   geom_point()+
#   ylab(var)

# model Stem height---------------------------------------------------------------------
#' Stem height

stem_height=function(X,slope1,slope2,breakMAP){
  int=breakMAP*(slope1-slope2)
  ifelse(X<breakMAP,slope1*X,slope2*X+int)
}

MAP_sim=seq(0,600,1)
breakMAP=5*12

donStem=don%>%filter(Variable=='StemHeight')

modStem=nls(data = don%>%
      filter(Variable=='StemHeight'),
    formula =
      Measurement ~ stem_height(X= MAP, slope1= slope1,slope2=slope2,breakMAP=breakMAP),
    start= c(slope1= 1,slope2=1),trace = F)

# ### verif
# donStemSim=data.frame(MAP=MAP_sim)%>%
#   mutate(sim=stem_height(X = MAP,slope1= coef(modStem)['slope1'],slope2=coef(modStem)['slope2'],breakMAP=breakMAP))
#   
# ggplot()+
#   geom_point(data=donStem,aes(x=MAP,y=Measurement))+
#   geom_line(data=donStemSim,aes(x=MAP,y=sim,col='sim'))



# model rachis length -----------------------------------------------------
sigmo=function(x,max,slp,infl){
  max/(1+exp(4*slp*(infl-x)))
}

donRac=don%>%filter(Variable=='FrondLength')%>%
  mutate(nbLeavesEmitted=2.16*MAP)

modRac=nls(data = donRac,
           formula =
             Measurement ~ sigmo(x= nbLeavesEmitted, max=max,slp=slp,infl=infl),
           start= c(max=700,slp=0.01,infl=150),trace = F)

# ### verif
# donRacSim=data.frame(nbLeavesEmitted=MAP_sim)%>%
#   mutate(sim=sigmo(x=nbLeavesEmitted,max=coef(modRac)['max'],slp=coef(modRac)['slp'],infl=coef(modRac)['infl']))
# 
# ggplot()+
#   geom_point(data=donRac,aes(x=nbLeavesEmitted,y=Measurement))+
#   geom_line(data=donRacSim,aes(x=nbLeavesEmitted,y=sim,col='sim'))
# 


# ratio length weight -----------------------------------------------------

rat=fread("0-data/LeafMorpho.csv")


rat%>%
  ggplot(aes(x=RachisFW,y=RachisLength))+
  geom_point()


rat%>%
  ggplot(aes(x=RachisLength,y=WidthC))+
  geom_point()

rat%>%
  ggplot(aes(x=RachisLength,y=ThickC/WidthC))+
  geom_point()

lm(data = rat,RachisLength~RachisFW)
