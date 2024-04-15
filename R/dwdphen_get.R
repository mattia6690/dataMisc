# Dowlonad the DWD phenological data
# Bulk downloader


# Initialization ----------------------------------------------------------
# Libraries
library("rdwd")
library("rvest")
library("tidyverse")

# Directories
hst.dir  <- bind_cols(Timeframe=c("recent","historical"),Dir=c("DWDdata/Phenology/recent/","DWDdata/Phenology/historical/"))
rec.dir  <- "DWDdata/Phenology/recent/"
ts.dir   <- "DWDdata/Phenology/timeseries/"
meta.dir <- "DWDdata/Phenology/metadata/"

# Levels in the FTP server
base<-sub("climate$", "phenology", dwdbase)
l1<-c("annual_reporters","immediate_reporters")
l2<-c("fruit","crops")
l3<-c("recent","historical")


# Structure  --------------------------------------------------------------
# Link the several levels
dwd.phen.url<-expand.grid(L0_URL=base, reporter=l1,Type1=l2,Timeframe=l3,stringsAsFactors = F) %>% 
  mutate(L1_URL=paste(L0_URL,reporter,Type1,Timeframe,sep="/")) %>% 
  as_tibble

# String of crops to retrieve
# German is for Linkage to DWD and English is the final translation
string.sel <-c("Apfel","Dauergruenland","Mais","Sommergerste","Kartoffel","Zucker-Ruebe",
               "Wintergerste","Winterraps","Winterroggen","Winterweizen")
string.sel.eng<-c("Apple","Grassland","Maize","SpringBarley","Potato","SugarBeet",
                  "WinterBarley","WinterRapeseed","WinterRye","WinterWheat")

# Read the FTP server and data structure
dwd.phen.urls<-dwd.phen.url %>% 
  mutate(L2_name=map(L1_URL,function(x){
    
    # Read Page
    page     <- rvest::read_html(x)
    pagetext <- rvest::html_text2(page) %>% 
      strsplit(.," ") %>% 
      unlist
    
    # Get subsections
    pagetext.melder     <- pagetext[which(substr(pagetext,1,4)=="PH_J" | substr(pagetext,1,4)=="PH_S")]
    pagetext.melder.sub <- gsub("[\r\n]"," ",pagetext.melder)
    pagetext.melder.sub <- sapply(strsplit(pagetext.melder.sub," "), `[`, 1)
    
    # Bind the crops and subsection
    classes<-sapply(pagetext.melder.sub,function(x){croptype<-string.sel.eng[str_detect(x,string.sel)]})
    binder<- unnest(enframe(classes,name="L2_name","Type2"),cols = c(Type2))
    
    
    
    
    return(binder)
    
  })) %>% 
  unnest(c(L2_name)) %>% 
  mutate(L2_URL=paste(L1_URL,L2_name,sep="/"))

# Add the name of the ourput
dwd.phen<- dwd.phen.urls %>% 
  select(Type1,Type2,Timeframe,L2_URL) %>% 
  mutate(Output=paste("DWDdata/Phenology",Type1,Type2,Timeframe,basename(L2_URL),sep="/"))

# Delete the Spezifizierung in Maize
dwd.phen<-dwd.phen[-which(str_detect(dwd.phen$L2_URL,"Spezifizierung")),]



# Create the Directories if not present already
uout<-unique(dirname(dwd.phen$Output))
s<-sapply(uout,function(x) dir.create(x,recursive=T,showWarnings = F))

# Download ----------------------------------------------------------------

for(i in 1:nrow(dwd.phen)){
  
  # Is it an historical tieframe and was it downloaded?
  tf_hist<-dwd.phen$Timeframe[i]=="historical"
  tf_there<-file.exists(dwd.phen$Output[i])
  tf_day <- difftime(Sys.time(),file.info(dwd.phen$Output[i])$mtime,units="days")<1
  
  if(tf_hist & tf_there) {
    print("Historical Dataset was downloaded already - skipping")
    next()
    
  } else if (tf_there & tf_day) {
    print("Downloaded less than 1 day ago - skipping")
    next()
  } else {
    
    download.file(dwd.phen$L2_URL[i],dwd.phen$Output[i],quiet = T)
    print(paste(dwd.phen$Output[i],"- downloaded"))
    
  }
}


# Metadata ----------------------------------------------------------------
# Crops

# List of metadata files
crop.stations<-"ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/annual_reporters/crops/historical/PH_Beschreibung_Phaenologie_Stationen_Jahresmelder.txt"
crop.phases<-"ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/annual_reporters/crops/historical/PH_Beschreibung_Phasendefinition_Jahresmelder_Landwirtschaft_Kulturpflanze.txt"
fruit.stations<-"ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/annual_reporters/fruit/historical/PH_Beschreibung_Phaenologie_Stationen_Jahresmelder.txt"
fruit.phases<-"ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/annual_reporters/fruit/historical/PH_Beschreibung_Phasendefinition_Jahresmelder_Obst.txt"

meta.crops<-bind_cols(Type1="crops",Type2=c("Stations","Phases"),L2_URL=c(crop.stations,crop.phases))
meta.fruit<-bind_cols(Type1="fruit",Type2=c("Stations","Phases"),L2_URL=c(fruit.stations,fruit.phases))

# Tidy and Output
meta<-bind_rows(meta.crops,meta.fruit) %>% 
  mutate(Output=paste0(meta.dir,Type1,"_",Type2,".csv")) %>% 
  mutate(data=map2(L2_URL,Type2,function(x,y) {
    
    # Read and delete White spaces
    tib<-as_tibble(read.table(x, sep=";", header=TRUE))
    tib2<-mutate_if(tib,is.character, trimws)
    
    # Select the right columns
    if(y=="Stations") ret<-select(tib2,Stations_id,Stationsname,Lat="geograph.Breite",Lon="geograph.Laenge",Bundesland) 
    if(y=="Phases") ret<-select(tib2,Objekt_id,Objekt,Phase_id=Phasen_id,Phase,BBCH_Code)
    
    return(unique(ret))
    
    }))

# Download
for(i in 1:nrow(meta)) write.csv(meta$data[i],meta$Output[i])

# TimeSeries --------------------------------------------------------------------
# Tidy the timeseries
dwd.phen2<-dwd.phen %>% 
  select(Type1,Type2,Output) %>% 
  nest(dataRaw=Output)

# Download the timeseries
for(i in 1:nrow(dwd.phen2)) {
  
  # Read
  l<-lapply(dwd.phen2$dataRaw[[i]]$Output, function(x) read.table(x, sep=";", header=TRUE))
  
  
  #l<-suppressMessages(lapply(dwd.phen2$dataRaw[[i]]$Output, read_csv2))
  lbind<-unique(do.call(bind_rows,l))
  lbind.sort<-arrange(lbind,Stations_id,Eintrittsdatum) %>% 
    mutate(Croptype=dwd.phen2$Type2[i]) %>% 
    as_tibble %>% 
    select(-c(eor,X,Eintrittsdatum_QB))
  
  
  # Add metadata
  stations<-read.csv(paste0("DWDdata/Phenology/metadata/",dwd.phen2$Type1[i],"_Stations.csv"))
  phases<-read.csv(paste0("DWDdata/Phenology/metadata/",dwd.phen2$Type1[i],"_Phases.csv"))
  
  lj1<-left_join(lbind.sort,stations, by = "Stations_id")
  lj2<-left_join(lj1,phases, by = c("Objekt_id","Phase_id"))
  
  # Download
  out<-paste0(ts.dir,dwd.phen2$Type2[i],"_timeseries_",format(Sys.Date(),"%Y%m%d"),".csv")
  write_csv(lj2,out)
  
  # Housekeeping
  rm(l,lbind,lbind.sort,lj2)
  print(paste(out,i))
  
}






