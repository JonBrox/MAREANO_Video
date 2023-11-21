require(readxl)
require(worms)
require(worrms)

Not_Species <- c("transparent","orange", "red", "redish", "pink","yellow", "white", "dark","violet","brown" , "shiny","blue","bluegrey","green","grey","purple",
                 "fan", "egg", "encrusting", "branched","branch", "trunk","urn","urn-shaped", "tubes","tube","mat", "cupcake", "window", "lily", "bat","erect",
                 "round","cup","stalked","colonial","stolon","big", "regular", "irregular", "gyro","bush","bottlebrush","parabol", "feather","tree", "lollipop","string",
                 "medium","small","busk","agglutinated","thick",
                 "bladformet","trådformet", "shaped", "papillae","soft",
                 "buried",  "benthic","sand", "sediment","dirty", "calcareous", "solitary", "colonial", "coral","epizoic", "plus", "pelagic","surfing", "edges",
                 "sp", "mark", "fragment", "light", "impacted", "spikey")

Not_Animals<-c("Algae","Laminariales","Rhodophyta" ,"Seaweed","Phaeophyceae","Kelp","alger","Chlorophyta", #Plants 
               "Lebensspuren","Mergel" ,"Burrow","Burrows", "burrow","Bacterial", #Features
               "question", "løsliggende", "Biobush", "Biota", "Unidentified","Egg", "Eggs", "juvenile","fishingnet", "Litter", "tube", "Trawl", "sand", "rock") #Others

SpAphia_m<-str_split_fixed(colnames(otuSel), "[._; ]",4)
SpAphia_m<-cbind(SpAphia_m, Aphias = "", Aphias2 = "", Aphias3 = "")
if(exists("Species2")){rm(Species2,Species3)}
for(i in 1:nrow(SpAphia_m)){
  
  Species<-paste(SpAphia_m[i,which(!(SpAphia_m[i,] %in% c(Not_Species, Not_Animals)))], collapse =" ")
  Species<-str_trim(Species)
  if(Species %in%  c("NA", "White dot on ")){next}
  
  if(Species == "Echinoidea" & SpAphia_m[i,2] == "irregular"){Species <- "Irregularia"}
  if(Species == "Echinoidea" & SpAphia_m[i,2] == "regular"){Species <- "Carinacea"}
  
  # BRACKETS
  if(Species == "Porania pulvillus"){Species<- "Porania (Porania) pulvillus"}
  if(Species == "Poraniomorpha tumida"){Species<- "Poraniomorpha (Poraniomorpha) tumida"}  
  if(Species == "Antho dichotoma"){Species<- "Antho (Antho) dichotoma"}  
  if(Species == "Asbestopluma furcata"){Species<- "Asbestopluma (Asbestopluma) furcata"}  
  if(Species == "Asbestopluma pennatula"){Species<- "Asbestopluma (Asbestopluma) pennatula"}  
  if(Species == "Caulophacus arcticus"){Species<- "Caulophacus (Caulophacus) arcticus"}  
  if(Species == "Chondrocladia grandis"){Species<- "Chondrocladia (Chondrocladia) grandis"}  
  if(Species == "Hamacantha bowerbanki"){Species<- "Hamacantha (Vomerula) bowerbanki"}  
  if(Species == "Hymedesmia paupertas"){Species<- "Hymedesmia (Hymedesmia) paupertas"}  
  if(Species == "Latrunculia"){Species<- "Latrunculia (Latrunculia)"}  
  if(Species == "Mycale lingua"){Species<- "Mycale (Mycale) lingua"}  
  if(Species == "Leptasterias muelleri"){Species<- "Leptasterias (Leptasterias) muelleri"}  
  
  # IMPERFECT NOTATIONS
  if(Species == "Ceramaster Hippasterias"){
    Species <- "Ceramaster"
    Species2 <- "Hippasteria"
  }
  if(Species == "Porania Poraniomorpha"){
    Species <- "Porania"
    Species2 <- "Poraniomorpha"
  }  
  if(Species == "Bythocaris Boreomysis"){
    Species <- "Bythocaris"
    Species2 <- "Boreomysis"
  } 
  if(Species == "Lycodonus Lycenchelys Lumpenus"){
    Species <-"Lumpenus"
    Species2 <- "Lycenchelys"
    Species3 <- "Lycodonus"
  } 
  if(Species == "Lycodonus Lycenchelys"){
    Species <- "Lycodonus"
    Species2 <- "Lycenchelys"
  } 
  if(Species == "Phakellia Axinella"){
    Species <- "Phakellia"
    Species2 <- "Axinella"
  } 
  if(Species == "Geodia Stryphnus"){
    Species <- "Geodia"
    Species2 <- "Stryphnus"
  }
  if(Species == "Geodia Stelleta"){
    Species <- "Geodia"
    Species2 <- "Stelleta"
  }  
  
  if(Species == "Cup"){Species <- "Scleractinia"}
  
  # OLD NAMES
  if(Species == "Gorgonacea"){Species <- "Octocorallia"}
  if(Species == "Alcyoniina"){Species <- "Malacalcyonacea"}
  if(Species == "Lophelia pertusa"){Species<- "Desmophyllum pertusum"}  
  if(Species == "Steletta grubei"){Species <- "Stelletta grubii"}
  if(Species == "Prosobranchia"){Species<- "Gastropoda"}  
  
  #AphiaIDs<-rbind(AphiaIDs, wm_name2id(Species))
  if(Species == ""){next}
  
  WormsRecord<-wm_records_names(Species)[[1]]
  
  if(any(WormsRecord$status %in%  "accepted")){
    WormsRecord<-WormsRecord[WormsRecord$status == "accepted",]
  }else{
    WormsRecord<-wm_records_names(WormsRecord$valid_name)[[1]]
    WormsRecord<-WormsRecord[WormsRecord$status == "accepted",]
  }
  #  print(WormsRecord)
  SpAphia_m[i,5]<- paste(WormsRecord[1,1])
  
  if(exists("Species2")){
    WormsRecord2<-wm_records_names(Species2)[[1]]
    WormsRecord2<-WormsRecord2[WormsRecord2$status == "accepted",]
    SpAphia_m[i,6]<- paste(WormsRecord2[1,1])
    rm(Species2)
    if(exists("Species3")){
      WormsRecord3<-wm_records_names(Species3)[[1]]
      WormsRecord3<-WormsRecord3[WormsRecord3$status == "accepted",]
      SpAphia_m[i,7]<- paste(WormsRecord3[1,1])
      rm(Species3)
    }
  }
}

SpAphia_df<-data.frame(SpAphia_m)
SpAphia_df$Aphias<-as.numeric(paste(SpAphia_df$Aphias))


