###############
# Script to generate a average meteo day  ----------------------------
#source. https://open-meteo.com/en/docs/#latitude=17.1814&longitude=-93.2744&hourly=temperature_2m,relative_humidity_2m,direct_normal_irradiance,direct_radiation_instant,diffuse_radiation_instant,direct_normal_irradiance_instant,terrestrial_radiation_instant&daily=sunrise,sunset,daylight_duration&timezone=auto&start_date=2020-01-01&end_date=2024-01-01&time_mode=time_interval
################
# R.PEREZ February 2024


# Load packages -----------------------------------------------------------
packs <- c("lubridate", "stringr", 'tidyverse','viridis','data.table')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)


# load data ---------------------------------------------------------------

met_raw=data.table::fread(input = '0-data/open-meteo-17.00N93.38W622m.csv',skip = 3)%>%
  na.omit()

met=met_raw%>%
  mutate(time=ymd_hm(str_replace(time,pattern='T',replacement='')),
         date=date(time),
         day=day(time),
         month=month(time),
         year=year(time),
         hour=hour(time))

met=met%>%
  group_by(date)%>%
  mutate(nbHours=n())%>%
  filter(nbHours==24) ## keep full day


met %>% 
  ggplot(aes(x=hour,y=`direct_radiation_instant (W/m²)`))+
  geom_line(aes(group=date,col=date))

met %>% 
  ggplot(aes(x=hour,y=`relative_humidity_2m (%)`))+
  geom_line(aes(group=date,col=date))

met %>% 
  ggplot(aes(x=hour,y=`temperature_2m (°C)`))+
  geom_line(aes(group=date,col=date))

met %>% 
  ggplot(aes(x=hour,y=`wind_speed_10m (km/h)`))+
  geom_line(aes(group=date,col=date))


# average -----------------------------------------------------------------

metAv=met %>% 
  group_by(hour)%>%
  summarise(hour=mean(hour),
    date=median(date),
            temperature=mean(`temperature_2m (°C)`),
            relativeHumidity=mean(`relative_humidity_2m (%)`),
            VPD=mean(`vapour_pressure_deficit (kPa)`),
            Ri=mean(`direct_radiation_instant (W/m²)`),
            wind=mean(`wind_speed_10m (km/h)`),
            atmosphereCO2_ppm=400)%>%
  ungroup()%>%
  data.frame()%>%
  select(date,hour,temperature,relativeHumidity,VPD,Ri,wind,atmosphereCO2_ppm)
  
data.table:fwrite(metAv,file = '0-data/meteoCampeche.csv',row.names =F)
