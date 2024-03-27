

# Initialization ----------------------------------------------------------
# Load the libraries
library("rdwd")
library("tidyverse")
library("sf")
library("lubridate")
library("mapview")

# Get the metadata
data(metaIndex)
dwdmeta<-metaIndex %>% 
  select(STATIONS_ID=Stations_id  ,Elevation=Stationshoehe,Lat=geoBreite,Lon=geoLaenge,Name=Stationsname,Region=Bundesland) %>% 
  unique %>% 
  as_tibble

# Get metadata for Station
dwd.meta.spat<-sf::st_as_sf(dwdmeta,coords=c("Lon","Lat"),crs=4326)
# write_sf(dwd.meta.spat,"RoM/data/DWDdata_stations.shp")

# Soil Temperature --------------------------------------------------------
lf<-list.files("DWDdata/SoilT/",full.names=T)

lp<-lapply(lf,function(x){
  
  out<-rdwd::readDWD(x,quiet = T) %>% 
    as_tibble() %>% 
    select(STATIONS_ID,MESS_DATUM,V_TE002M,V_TE005M,,V_TE010M,V_TE020M,V_TE050M) %>% 
    mutate(Date=as_date(MESS_DATUM))
  
})

lp.bind<-do.call(rbind,lp)
lp.bind.group<-lp.bind %>% 
  mutate(Date=as_date(MESS_DATUM)) %>% 
  group_by(STATIONS_ID,Date) %>% 
  summarise(TE002_min=min(V_TE002M),
            TE002_max=max(V_TE002M),
            TE002_mean=mean(V_TE002M),
            TE005_min=min(V_TE005M),
            TE005_max=max(V_TE005M),
            TE005_mean=mean(V_TE005M),
            TE010_min=min(V_TE010M),
            TE010_max=max(V_TE010M),
            TE010_mean=mean(V_TE010M),
            TE020_min=min(V_TE020M),
            TE020_max=max(V_TE020M),
            TE020_mean=mean(V_TE020M))

lp.fin<-lp.bind.group %>% ungroup %>% 
  mutate(Year=year(Date)) %>% 
  filter(Year>2010) %>% 
  select(-c(TE002_min,TE002_mean,TE002_max,Year))

write.csv(lp.fin,"RoM/data/DWDdata_Allard_2010.csv")

# Air Temperature ---------------------------------------------------------

start=2010
lf_temp<-list.files("DWDdata/AirT//",full.names=T)

lf_temp_rel<-sapply(lf_temp,function(x){
  
  r<-str_split(x,pattern="_")
  r<-as.numeric(substr(r[[1]][9],1,4))
  return(r)
  
})

lf_temp_relnum<-as.numeric(lf_temp_rel)
airdata<-lf_temp[which(lf_temp_relnum>start)]

lp_temp<-lapply(airdata,function(x){
  
  out<-rdwd::readDWD(x,quiet = T,fread = T) %>% 
    as_tibble() %>% 
    mutate(Year=year(MESS_DATUM)) %>% 
    filter(Year>start) %>% 
    mutate(DOY=yday(MESS_DATUM)) %>% 
    mutate(Hour=hour(MESS_DATUM)) %>% 
    select(STATIONS_ID,Year,DOY,Hour,TT_TER)
  
  return(out)
  
})

lp_temp.bind<-do.call(rbind,lp_temp)
write.csv(lp_temp.bind,"RoM/data/DWDtemp_2010.csv",row.names = F)


# Snow Cover --------------------------------------------------------------


lf_snow<-list.files("DWDdata/SnowD/",full.names=T)
lp_snow<-lapply(lf_snow,function(x){
  
  out<-rdwd::readDWD(x,quiet = T)
  out<-out[!is.na(out$SH_TAG),]
  out<-as_tibble(out) %>% 
    mutate(Year=year(MESS_DATUM)) %>% 
    filter(Year>2010) %>% 
    select(STATIONS_ID,MESS_DATUM,SH_TAG)
  
  return(out)
  
})

lp_snow.bind<-do.call(rbind,lp_snow)
lp_snow.bind2<-lp_snow.bind %>% 
  mutate(Year=year(MESS_DATUM)) %>% 
  mutate(DOY=yday(MESS_DATUM)) %>% 
  select(-MESS_DATUM) %>% 
  select(STATIONS_ID,Year,DOY,SH_TAG)
write.csv(lp_snow.bind2,"RoM/data/DWDsnow_2010.csv",row.names = F)




dn.tib<-as_tibble(dn)
tidy.data<-left_join(dn.tib,dwdmeta,by="STATIONS_ID")

start<-"2011-09-01"
end  <-"2012-05-01"


t1   <- lp[[467]] %>% filter(Date>=start & Date<end)





stat <- lp.spat.com %>% filter(STATIONS_ID==unique(lp[[467]]$STATIONS_ID))
t2<-t1 %>% 
  select(Date,V_TE005M) %>% 
  mutate(t=as.character(Date)) %>% 
  mutate(crownTemp=as.character(V_TE005M)) %>% select(t,crownTemp) %>% as.data.frame


source("RoM/initModel.R")
source("wcsmR2_functions.R")
jdays <- JD(strptime(t1$Date,format='%Y-%m-%d'))
dlgt  <- daylength(stat$Lat[[1]],stat$Lon[[1]],jdays,-6)[,3]
model(t="2012-02-06",Y=Y,parameters = parms,crownTemps = t1$V_TE005M,daylengths = dlgt)


DELAY(t,10,t2$crownTemp)



# Plots -------------------------------------------------------------------

t1.plot<-pivot_longer(t1,cols=c(V_TE002M,V_TE005M,V_TE010M,V_TE020M,V_TE050M),names_to="Depth",values_to = "DegreesC")

ggplot(t1.plot,aes(Date,DegreesC,col=Depth))+
  geom_line()+
  ggtitle("Soil Temperature in different depths",
          subtitle = "Hoyerswerda, Sachsen, Germany")+
  labs(caption = "(Data acquired from the German Weatherservice (DWD))")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") 

