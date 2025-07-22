# Tidy the downloaded DWD data
library("tidyverse")
download_and_format_dwd<-function(url){
  
  # 2. Read lines with correct encoding and drop blanks
  lines <- read_lines(url, locale = locale(encoding = "Latin1")) %>% 
    paste(.,collapse="") %>% 
    str_replace_all(.,"  ","") %>%
    str_replace_all(.,"\t","") %>% 
    str_replace_all(.,"; ",";") %>% 
    str_replace_all(.," ;",";")
    
  
  fields <- str_split(lines, ";")[[1]]
  afterheader<-which(!is.na(as.numeric(fields)))[1]
  columns <- fields[1:(afterheader-1)]
  
  data_fields <- fields[-(1:length(columns))] 
  data_fields <- data_fields[-length(data_fields)]# remove header
  n_cols <- length(columns)
  
  # 5. Break into rows (every 8 fields == 1 record)
  rows <- matrix(data_fields, ncol = n_cols, byrow = TRUE)
  colnames(rows) =columns
  rowstib<-as_tibble(rows) %>% type_convert()
  
  return(rowstib)
  
}

dwd.phen<-readRDS("DWDdata/Phenology/DWDphentable.rds")
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
  mutate(data=list(NA))


for(i in 1:nrow(meta)){
  
  # tib<-as_tibble(read.table(meta$L2_URL[i], sep=";",header=T,fill=T))
  # tib2<-tib %>% 
  #   mutate_if(is.character, utf8::utf8_encode) %>% 
  #   mutate_if(is.character, trimws) %>% 
  #   mutate_if(is.character, ~str_replace_all(., "<fc>", "ü")) %>% 
  #   mutate_if(is.character, ~str_replace_all(., "<e4>", "ä")) %>% 
  #   mutate_if(is.character, ~str_replace_all(., "<f6>", "ö")) %>% 
  #   mutate_if(is.character, ~str_replace_all(., "<c4>", "Ä")) %>% 
  #   mutate_if(is.character, ~str_replace_all(., "<df>", "ss"))
  # 
  
  tib2<-download_and_format_dwd(meta$L2_URL[i])
  
  
  if(meta$Type2[[i]]=="Stations") {
    
    ret<-select(tib2,Stations_id,Stationsname,Lat="geograph.Breite",Lon="geograph.Laenge",Bundesland) 
    
  }
  
  if(meta$Type2[[i]]=="Phases") {
    ret<-select(tib2,Objekt_id,Objekt,Phase_id=Phasen_id,Phase,BBCH_Code)
    
    # Crops
    ret$BBCH_Code[str_detect(ret$Phase,"Bestellung")]=0
    ret$BBCH_Code[str_detect(ret$Phase,"Auflaufen")]=10
    ret$BBCH_Code[str_detect(ret$Phase,"Schossen")]=31
    ret$BBCH_Code[str_detect(ret$Phase,"Ährenschieben")]=51
    ret$BBCH_Code[str_detect(ret$Phase,"Vollblüte")]=65
    ret$BBCH_Code[str_detect(ret$Phase,"Milchreife")]=75
    ret$BBCH_Code[str_detect(ret$Phase,"Gelbreife")]=87
    ret$BBCH_Code[str_detect(ret$Phase,"Vollreife")]=87
    ret$BBCH_Code[str_detect(ret$Phase,"Ernte")]=99
    
    # Grassland
    ret$BBCH_Code[str_detect(ret$Phase,"Ergrünen")]=0
    ret$BBCH_Code[str_detect(ret$Phase,"1. Heuschnitt")]=98
    ret$BBCH_Code[str_detect(ret$Phase,"1. Silageschnitt")]=98
    ret$BBCH_Code[str_detect(ret$Phase,"1. Heu- oder ")]=98
    ret$BBCH_Code[str_detect(ret$Phase,"2. Heuschnitt")]=99
    ret$BBCH_Code[str_detect(ret$Phase,"2. Silageschnitt")]=99
    ret$BBCH_Code[str_detect(ret$Phase,"2. Heu- oder ")]=99
    
    # Potato and Fruits
    ret$BBCH_Code[str_detect(ret$Phase,"Bestand geschlossen")]=35
    ret$BBCH_Code[str_detect(ret$Phase,"Blüte Beginn")]=61
    ret$BBCH_Code[str_detect(ret$Phase,"Blüte Ende")]=69
    ret$BBCH_Code[str_detect(ret$Phase,"Pflückreife Beginn")]=87
    
    # Vegetables
    ret$BBCH_Code[str_detect(ret$Phase,"Pflanzen Beginn")]=0
    
    ret$NewPhase=paste(ret$BBCH_Code,ret$Phase,sep="-")
    
    
  }
  
  meta$data[[i]]<-unique(ret)
  
}

# Write to Disk
for(i in 1:nrow(meta)) write.csv(meta$data[[i]],meta$Output[[i]], row.names = FALSE)

# TimeSeries --------------------------------------------------------------------
# Tidy the timeseries
dwd.phen2<-dwd.phen %>% 
  select(Type1,Type2,reporter,Output) %>% 
  nest(dataRaw=-c(Type1,Type2))

# Read the dataset
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
  out<-paste0(ts.dir,dwd.phen2$Type2[i],"_timeseries.csv")
  write.csv(lj2,out,row.names=F)
  
  # Housekeeping
  rm(l,lbind,lbind.sort,lj2)
  print(paste(out,i))
  
}



