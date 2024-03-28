# Plot the Phenological obserevations

library("sf")
library("tidyverse")
library("lubridate")
library("giscoR")
library("ggpubr")

nuts_DE0<-gisco_get_countries(country="DE")

phenfiles<-list.files("DWDdata/Phenology/timeseries/",full.names=T)


for(i in 1:length(phenfiles)){
  
  cfile<-read_csv(phenfiles[i],show_col_types = FALSE) %>% 
    filter(!is.na(Lat)& !is.na(Lon)) %>% 
    mutate(BBCHPhase=paste(BBCH_Code,Phase,sep="-")) %>% 
    st_as_sf(coords=c("Lon","Lat"),crs=4326)
  
  # Current -----------------------------------------------------------------
  
  refyear=year(Sys.Date())
  refyear=2024
  
  refyear.stat= refyear-2
  refyear.mta = refyear-10
  
  cur<-cfile %>% filter(Referenzjahr==refyear) %>% mutate(year=as.character(Referenzjahr)) %>% 
    filter(BBCHPhase!="NA-NA")
  
  if(nrow(cur)<1) next
  
  last5<-cfile %>% filter(Referenzjahr<refyear & Referenzjahr>=refyear.from) %>% select(Stations_id,Stationsname,geometry) %>% unique %>% 
    mutate(TimeRange=paste(refyear.from,"to",refyear))
  
  stat.has<-length(unique(cur$geometry))
  stat.sld<-nrow(last5)
  stat.has.perc<-round((stat.has/stat.sld)*100)
  
  g1<-ggplot()+ theme_light()+
    geom_sf(data=nuts_DE0)+
    geom_sf(data=last5,col="grey70")+
    geom_sf(data=cur,aes(fill=Jultag),shape=21,size=2)+
    facet_wrap(.~BBCHPhase,ncol=4)+
    labs(fill="DOY")+
    scale_fill_gradientn(colors=rainbow(100),limits = c(0, 360),breaks=seq(0,360,90))+
    ggtitle(paste("Observations",refyear))+
    theme(panel.border=element_blank(),
          panel.grid = element_blank(),
          axis.text.x= element_blank(),
          axis.text.y = element_blank())+
    theme(legend.position="bottom", legend.box = "horizontal")
  
  
  
  # MTA ---------------------------------------------------------------------
  
  lta <- cfile %>% filter(Referenzjahr<refyear & Referenzjahr>=refyear.mta) %>% 
    group_by(Stations_id,BBCHPhase,geometry) %>% 
    summarize(minDOY=min(Jultag),
              maxDOY=max(Jultag),
              meanDOY=mean(Jultag),
              n=n(),
              lastObsYear=max(Referenzjahr)) %>% 
    filter(n>5) %>% 
    as_tibble() %>% 
    select(-geometry)
  
  
  jn<-left_join(cur,as_tibble(lta),by=c("Stations_id","BBCHPhase")) %>% 
    filter(!is.na(n)) %>% 
    mutate(Diff=Jultag-meanDOY)
  
  
  g2<-ggplot()+ theme_light()+
    geom_sf(data=nuts_DE0)+
    geom_sf(data=last5,col="grey70")+
    geom_sf(data=jn,aes(fill=Diff),shape=21,size=2)+
    facet_wrap(.~BBCHPhase,ncol=4)+
    labs(fill="Difference in Days")+
    scale_fill_gradientn(colors=c("Darkgreen","Green","Yellow","Orange","Red"),limits=c(-50,50))+
    ggtitle(paste("Difference to MTA"))+
    theme(panel.border=element_blank(),
          panel.grid = element_blank(),
          axis.text.x= element_blank(),
          axis.text.y = element_blank())+
    theme(legend.position="bottom", legend.box = "horizontal")
  
  
  # Distribution -------------------------------------------------------------------
  
  dist <- cfile %>% filter(Referenzjahr>=refyear.mta) %>% mutate(year=as.character(Referenzjahr))
  
  g3<-ggplot(dist,aes(x=Jultag,fill=BBCHPhase))+ theme_light()+
    geom_bar()+
    facet_wrap(.~Referenzjahr)+
    xlab("DOY")+
    ylab("Count")+
    ggtitle(paste("DWD Phenology -",unique(cur$Croptype)),
            paste("With Data until", unique(max(cur$Eintrittsdatum))))+
    labs(fill="<BBCH>-<Phase DE>")+
    theme(plot.title = element_text(size = 20, face = "bold"))+
    scale_x_continuous(limits=c(0,360),breaks=seq(0,360,30))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
  gg<-ggarrange(g3,ggarrange(g1,g2,ncol=2),nrow=2)
  
  
  outdir<-paste0("DWDdata/Phenology/output/",refyear)
  outname<-paste0(outdir,"/DWD_Phenology_",unique(cur$Croptype),".jpg")
  if(!dir.exists(outdir)) dir.create(outdir,recursive = T)
  
  ggsave(plot=gg,outname,width=10,height=8)
  
}
