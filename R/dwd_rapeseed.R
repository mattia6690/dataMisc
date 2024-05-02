# rapeseed Statistics ----------------------------------------------------------------
nuts_DE0<-gisco_get_countries(country="DE")
phenfile.rseed<-list.files("DWDdata/Phenology/timeseries/",full.names=T,pattern = "WinterRapeseed")
refyear<-2024


raps.file<-read_csv(phenfile.rseed) %>% 
  mutate(Phase_id=sprintf("%02d",Phase_id)) %>% 
  filter(Referenzjahr>2015) %>% 
  filter(BBCH_Code==61) %>% 
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
write.csv(raps.file.group.bb,"DWDdata/Phenology/temp/Rapeseed_Flowering_DWD.csv")





# Apples ------------------------------------------------------------------

phenfile.rseed<-list.files("DWDdata/Phenology/timeseries/",full.names=T,pattern = "Apple")
refyear<-2024

apple <- read_csv(phenfile.rseed) %>% 
  mutate(Phase_id=sprintf("%02d",Phase_id)) %>% 
  filter(Referenzjahr>2015) %>% 
  rename(DOY=Jultag)

write_csv(apple,"DWDdata/Phenology/temp/ApplePhenology.csv")


