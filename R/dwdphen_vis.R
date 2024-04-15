# Plot the Phenological obserevations

library("sf")
library("tidyverse")
library("lubridate")
library("giscoR")
library("ggpubr")

nuts_DE0<-gisco_get_countries(country="DE")
phenfiles<-list.files("DWDdata/Phenology/timeseries/",full.names=T)
refyear<-2023


for(i in 1:length(phenfiles)){
  
  cfile<-read_csv(phenfiles[i],show_col_types = FALSE) %>% 
    mutate(Phase_id=sprintf("%02d",Phase_id)) %>% 
    filter(!is.na(Lat)& !is.na(Lon)) %>% 
    mutate(NewPhase=paste(Phase_id,Phase,sep="-")) %>% 
    st_as_sf(coords=c("Lon","Lat"),crs=4326)
  
  # Current -----------------------------------------------------------------
  
  if(is.na(refyear)) refyear=year(Sys.Date())
  refyear.stat= refyear-2
  
  cur<-cfile %>% 
    filter(Referenzjahr==refyear) %>% 
    mutate(year=as.character(Referenzjahr)) %>% 
    filter(NewPhase!="NA-NA")
  
  if(nrow(cur)<1) next
  
  last5<-cfile %>% 
    filter(Referenzjahr<refyear & Referenzjahr>=refyear.stat) %>% 
    select(Stations_id,Stationsname,geometry) %>% 
    unique %>% 
    mutate(TimeRange=paste(refyear.stat,"to",refyear))
  
  stat.has<-length(unique(cur$geometry))
  stat.sld<-nrow(last5)
  stat.has.perc<-round((stat.has/stat.sld)*100)
  
  g1<-ggplot()+ theme_light()+
    geom_sf(data=nuts_DE0)+
    geom_sf(data=last5,col="grey70")+
    geom_sf(data=cur,aes(fill=Jultag),shape=21,size=2)+
    facet_wrap(.~Phase_id,ncol=4)+
    labs(fill="DOY")+
    scale_fill_gradientn(colors=rainbow(100),limits = c(0, 360),breaks=seq(0,360,30))+
    ggtitle(paste("Observations",refyear),"Sorted by DWD Phase ID")+
    theme(panel.border=element_blank(),
          panel.grid = element_blank(),
          axis.text.x= element_blank(),
          axis.text.y = element_blank())+
    theme(legend.position="bottom", legend.box = "horizontal",legend.key.width = unit(0.7, "inch"),legend.key.height = unit(0.1, "inch"))
  
  
  # MTA ---------------------------------------------------------------------
  
  refyear.mta = refyear-10
  
  lta <- cfile %>% filter(Referenzjahr<refyear & Referenzjahr>=refyear.mta) %>% 
    group_by(Stations_id,NewPhase,geometry) %>% 
    summarize(minDOY=min(Jultag),
              maxDOY=max(Jultag),
              meanDOY=mean(Jultag),
              n=n(),
              lastObsYear=max(Referenzjahr)) %>% 
    filter(n>5) %>% 
    as_tibble() %>% 
    select(-geometry)
  
  
  jn<-left_join(cur,as_tibble(lta),by=c("Stations_id","NewPhase")) %>% 
    filter(!is.na(n)) %>% 
    mutate(Diff=Jultag-meanDOY)
  
  if(nrow(jn)<1) next
  
  g2<-ggplot()+ theme_light()+
    geom_sf(data=nuts_DE0)+
    geom_sf(data=last5,col="grey70")+
    geom_sf(data=jn,aes(fill=Diff),shape=21,size=2)+
    facet_wrap(.~Phase_id,ncol=4)+
    labs(fill="Days Difference")+
    scale_fill_gradientn(colors=c("Blue","Darkgreen","Green","Yellow","Orange","Red","Darkred"),
                         limits=c(-50,50),breaks=seq(-50,50,10))+
    ggtitle("Difference to MTA", "Sorted by DWD Phase ID")+
    theme(panel.border=element_blank(),
          panel.grid = element_blank(),
          axis.text.x= element_blank(),
          axis.text.y = element_blank())+
    theme(legend.position="bottom", legend.box = "horizontal",legend.key.width = unit(0.5, "inch"),legend.key.height = unit(0.1, "inch"))
  
  
  # Distribution -------------------------------------------------------------------
  
  dist <- cfile %>% 
    filter(Referenzjahr>=refyear.mta) %>%
    mutate(year=as.character(Referenzjahr))
  
  if(nrow(dist)<1) next
  
  g3<-ggplot(dist,aes(x=Jultag,fill=NewPhase))+ theme_light()+
    geom_bar()+
    facet_wrap(.~Referenzjahr)+
    xlab("DOY")+
    ylab("Count")+
    ggtitle(paste("DWD Phenology -",unique(cur$Croptype)),
            paste("With Data until", unique(max(cur$Eintrittsdatum))))+
    labs(fill="<PhaseID>-<Phase DE>")+
    theme(plot.title = element_text(size = 20, face = "bold"))+
    scale_x_continuous(limits=c(0,360),breaks=seq(0,360,30))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
  gg<-ggarrange(g3,ggarrange(g1,g2,ncol=2),nrow=2)
  
  
  outdir<-paste0("DWDdata/Phenology/output/",refyear)
  outname<-paste0(outdir,"/DWD_Phenology_",unique(cur$Croptype),".jpg")
  if(!dir.exists(outdir)) dir.create(outdir,recursive = T)
  
  ggsave(plot=gg,outname,width=10,height=8)
  
}
