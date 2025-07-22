# rapeseed Statistics ----------------------------------------------------------------
nuts_DE0<-gisco_get_countries(country="DE")
phenfile.rseed<-list.files("DWDdata/Phenology/timeseries/",full.names=T,pattern = "WinterRapeseed")
refyear<-2024


raps.file<-read_csv(phenfile.rseed) %>% 
  mutate(Phase_id=sprintf("%02d",Phase_id)) %>% 
  filter(Referenzjahr>2015) %>% 
  rename(DOY=Jultag)

raps.file.group<-raps.file %>% 
  group_by(Bundesland,Referenzjahr,BBCH_Code) %>% 
  nest %>% 
  mutate(Quantiles=map(data,function(x){
    
    quant<-quantile(x$DOY,seq(0,1,0.1),names=F) %>% t %>% as_tibble
    colnames(quant)=paste0("q",seq(0,100,10))
    return(quant)
  })) %>% 
  mutate(n=map_dbl(data,nrow)) %>% 
  select(-data) %>% 
  unnest(cols = c(Quantiles))


raps.file.group.bb<-raps.file.group %>% 
  filter(Bundesland=="Brandenburg") %>% 
  arrange(BBCH_Code,Referenzjahr) %>% 
  mutate(ProposedStart=round(q10-10)) %>% 
  mutate(ProposedEnd=round(q90+30))
write.csv(raps.file.group.bb,"DWDdata/Phenology/temp/Rapeseed_Flowering_DE4_Quantiles.csv")


raps_all<- raps.file %>% filter(Bundesland=="Brandenburg") %>% 
  select(Croptype,Year=Referenzjahr,DOY,Date=Eintrittsdatum,BBCH_Code,Lat,Lon)
write.csv(raps_all,"DWDdata/Phenology/temp/Rapeseed_Flowering_DE4_all.csv")


# * Results Fernando ------------------------------------------------------

library("readr")
library("tidyverse")
excelsheet<-"Analysis/Rapeseed_YellowIndex_DE4_Calibration_v3.xlsx"
data18<-readxl::read_xlsx(excelsheet,sheet=1)
data19<-readxl::read_xlsx(excelsheet,sheet=2)
data20<-readxl::read_xlsx(excelsheet,sheet=3)
data21<-readxl::read_xlsx(excelsheet,sheet=4)
data22<-readxl::read_xlsx(excelsheet,sheet=5)

data<-bind_rows(data18,data19,data20,data21,data22)

data %>% nest(-Year)

stats.all<-bind_cols(Year="All",
                     MedianYI=median(data$YellowIndex,na.rm=T),
                     SdYI=sd(data$YellowIndex,na.rm=T),
                     MedianObs=median(data$nobs,na.rm=T)) %>% 
  mutate(Threshold=MedianYI-SdYI)

stats.years<-data %>% 
  group_by(Year) %>% 
  summarize(MedianYI=median(YellowIndex,na.rm=T),
            SdYI=sd(YellowIndex,na.rm=T),
            MedianObs=median(nobs,na.rm=T)) %>% 
  mutate(Threshold=MedianYI-SdYI) %>% 
  mutate(Year=as.character(Year))


stats<-bind_rows(stats.all,stats.years)

ggplot(data,aes(x=YellowIndex))+ theme_bw()+
  geom_histogram(aes(y=..density..), fill="lightblue",bins=100)+
  geom_density()+
  facet_wrap(.~Year)+
  geom_vline(data=stats.years,aes(xintercept=MedianYI),col="blue",linetype=2)+
  geom_vline(data=stats.years,aes(xintercept=Threshold),col="darkgoldenrod",linetype=2)

# Apples ------------------------------------------------------------------

phenfile.rseed<-list.files("DWDdata/Phenology/timeseries/",full.names=T,pattern = "Apple")
refyear<-2024

apple <- read_csv(phenfile.rseed) %>% 
  mutate(Phase_id=sprintf("%02d",Phase_id)) %>% 
  filter(Referenzjahr>2015) %>% 
  rename(DOY=Jultag)

write_csv(apple,"DWDdata/Phenology/temp/ApplePhenology.csv")


