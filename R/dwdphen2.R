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
    mutate(Croptype=dwd.phen2$Type2[i])
  
  # Download
  out<-paste0(ts.dir,dwd.phen2$Type2[i],"_timeseries_",format(Sys.Date(),"%Y%m%d"),".csv")
  write_csv(lbind.sort,out)
  
  # Housekeeping
  rm(l,lbind,lbind.sort)
  print(paste(out,i))
  
}


sdown<-sapply(dwd.phen,function(x) download.file(x$L2_URL[1],x$Output[1]))

dwd.phen.url<-sub("climate$", "phenology", dwdbase) %>% 
  paste0(.,l1) %>% 
  paste0(.,l2) %>% 
  paste0(.,l3)

uls<-do.call(paste0, expand.grid(base, l1)) 
uls<-do.call(paste0, expand.grid(uls, l2))
uls<-do.call(paste0, expand.grid(uls, l3))

enframe(uls,name="ID",value="URL")


dwd.crop.urls <- c(paste0(dwd.phen.url,"/immediate_reporters/crops/recent/"),
                   paste0(dwd.phen.url,"/annual_reporters/crops/recent/"),
                   paste0(dwd.phen.url,"/immediate_reporters/crops/historical/"),
                   paste0(dwd.phen.url,"/annual_reporters/crops/historical/"))


a<-getURL(dwd.phen.url[1],header=T) 
tb = readHTMLTable(dwd.phen.url$baseurl[1])


for(i in 1:nrow(dwd.phen.url)){
  
  
  
  
}

page     <- rvest::read_html(dwd.phen.url$url[1])
pagetext <- rvest::html_text2(page) %>% 
  strsplit(.," ") %>% 
  unlist

pagetext.melder     <- pagetext[which(substr(pagetext,1,4)=="PH_J" | substr(pagetext,1,4)=="PH_S")]
pagetext.melder.sub <- gsub("[\r\n]"," ",pagetext.melder)
pagetext.melder.sub <- sapply(strsplit(pagetext.melder.sub," "), `[`, 1)





downlist<-list()
downlist$maize<- c("PH_Sofortmelder_Landwirtschaft_Kulturpflanze_Mais_ohne_Sortenangabe_akt.txt")


mon <- indexFTP(folder="/phenology/annual_reporters", dir=tempdir(), verbose=TRUE)

filenames = getURL(uls[1])
req = request(uls[1])
resp <- req %>% req_perform()
resp2<- resp %>% resp_raw()

out<-getURL(dwd.phen.url,verbose=TRUE,ftp.use.epsv=TRUE,dirlistonly = TRUE)


if(p.hst.down==T){
  
  for(i in 1:length(downlist){
    
    crop<-names(downlist)[i]
    from<-paste0(dwd.crop.urls[3:4],downlist[[i]])
    to<-  paste0(p.hst.dir,crop)
    
    
    
  })
  

  file<- dataDWD(base=pheno_url_rec, url=urls[i], joinbf=TRUE, 
                 dir=phenodir, read=FALSE) 
  
}
