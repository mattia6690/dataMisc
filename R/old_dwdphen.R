
# Get the phenology by the DWD


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

dwdmeta.lower<-dwdmeta
colnames(dwdmeta.lower)=tolower(colnames(dwdmeta))

phenodir<-paste0("DWDdata/Phenology/recent")
phenodir.meta<-paste0("DWDdata/Phenology/")


pheno_url_rec <- paste0(sub("climate$", "phenology", dwdbase), 
                         "/immediate_reporters/crops/recent/")
pheno_url_ann <- paste0(sub("climate$", "phenology", dwdbase), 
                         "/annual_reporters/crops/historical/")
# pheno_urls <- indexFTP("", base=phenocrop_base, dir="Pheno")
urls <- c("PH_Sofortmelder_Landwirtschaft_Kulturpflanze_Dauergruenland_akt.txt",
          "PH_Sofortmelder_Landwirtschaft_Kulturpflanze_Kartoffel_akt.txt",
          "PH_Sofortmelder_Landwirtschaft_Kulturpflanze_Mais_ohne_Sortenangabe_akt.txt",
          "PH_Sofortmelder_Landwirtschaft_Kulturpflanze_Sommergerste_akt.txt",
          "PH_Sofortmelder_Landwirtschaft_Kulturpflanze_Ruebe_akt.txt",
          "PH_Sofortmelder_Landwirtschaft_Kulturpflanze_Wintergerste_akt.txt",
          "PH_Sofortmelder_Landwirtschaft_Kulturpflanze_Winterraps_akt.txt",
          "PH_Sofortmelder_Landwirtschaft_Kulturpflanze_Winterroggen_akt.txt",
          "PH_Sofortmelder_Landwirtschaft_Kulturpflanze_Winterweizen_akt.txt")

urls.strings<-c("Grassland","Potato","MaisNoSort","Sommergerste","Sugar Beet",
                "Wintergerste","Winterraps","Winterroggen","Winterweizen")

files<-list()
for(i in 1:length(urls)){
  
  file<- dataDWD(base=pheno_url_rec, url=urls[i], joinbf=TRUE, 
                       dir=phenodir, read=FALSE) 
  file<- dataDWD(base=pheno_url_ann, url=urls[i], joinbf=TRUE, 
                 dir=phenodir, read=FALSE) 
  read <- read.table(file, sep=";", header=TRUE)
  colnames(read) =tolower(colnames(read))
  read<-read %>% mutate(FOI=urls.strings[i])
  files[[i]] =read
}

files.tib<-as_tibble(do.call(bind_rows,files))

basefile<-dataDWD(base=pheno_url_rec, 
                  url="PH_Beschreibung_Phasendefinition_Sofortmelder_Landwirtschaft_Kulturpflanze.txt", 
                  joinbf=TRUE, dir=phenodir.meta, read=FALSE) 

basefile.tib<- read.table(basefile, sep=";", header=TRUE) %>% as_tibble() %>% select(-Objekt)
colnames(basefile.tib)=tolower(colnames(basefile.tib))
basefile.red<-basefile.tib %>% select(objekt_id,phase_id=phasen_id,phase,bbch_code)

# Combine -----------------------------------------------------------------

stats<-dataDWD(base=pheno_url_ann, 
               url="PH_Beschreibung_Phaenologie_Stationen_Jahresmelder.txt", 
               joinbf=TRUE, dir=phenodir.meta, read=FALSE) 

stats.tib<-read.table(stats, sep=";", header=TRUE) %>% as_tibble() %>% select(1:4)  %>% 
  setNames(c("stations_id","stations_name","Lat","Lon"))

files.stat<-left_join(files.tib,stats.tib,by = "stations_id") %>% 
  left_join(.,basefile.red,by=c("objekt_id","phase_id"))





# Potato

files.pot<-files.stat %>% filter(FOI=="Potato")
files.pot.jn<-left_join(files.pot,basefile.red,by=c("objekt_id","phase_id"))
pot.10<-files.pot.jn %>% filter(phase_id==10)

ggplot(files.pot.jn,aes(x=referenzjahr)) +
  geom_bar()+
  facet_wrap(.~phase_id)+
  ggtitle(paste0("Potato Sowing Germany on ",format(Sys.Date(),"%d.%m.%y")),
          "Reported by DWD phenological observer")

ggplot(files.pot.jn,aes(x=jultag,fill=phase_id))+
  geom_bar()+
  facet_wrap(.~referenzjahr)+
  xlab("DOY")+
  ylab("Count")+
  ggtitle("DWD Phenological observer","Potato - Phase1 Seeding")


# Grassland

files.grs<-files.stat %>% filter(FOI=="Grassland")
files.grs.jn<-left_join(files.grs,basefile.red,by=c("objekt_id","phase_id"))
grs.10<-files.grs.jn %>% filter(phase_id==1) %>% mutate(referenzjahr=as.factor(referenzjahr))

ggplot(grs.10,aes(x=referenzjahr)) +
  geom_bar()+
  ggtitle(paste0("Green-up day in Grasslands in Germany as of ",format(Sys.Date(),"%d.%m.%y")),
          "Occurrences reported by DWD phenological observer")

ggplot(grs.10,aes(x=jultag))+
  geom_bar()+
  facet_wrap(.~referenzjahr)+
  xlab("DOY")+
  ylab("Count")+
  ggtitle("DWD Phenological observer","Grasslands - Phase1 Greenup")


# Beets

files.sbt<-files.stat %>% filter(FOI=="Sugar Beet")
files.sbt.jn<-left_join(files.sbt,basefile.red,by=c("objekt_id","phase_id"))
sbt.10<-files.sbt.jn %>% filter(phase_id==10)

ggplot(sbt.10,aes(x=referenzjahr)) +
  geom_bar()+
  ggtitle(paste0("Sugar Beet Sowing Germany on ",format(Sys.Date(),"%d.%m.%y")),
          "Reported by DWD phenological observer")

