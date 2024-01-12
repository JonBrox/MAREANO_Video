colnames(otuCompl) <-gsub(";", ".", colnames(otuCompl))
colnames(otuCompl) <-gsub("[ /]", "_", colnames(otuCompl))
otuSel <- otuCompl[,c(FALSE, colSums(otuCompl[,-c(1)] >0)>0)]

print("Searching for AphiaID for all species...")
source(paste0(PathScripts,"TaxonarySp_to_AphiaID.r"))
AphiaIDs<-SpAphia_df[,5]

print("Searching for Taxonomic crach for all Aphia IDs...")
Taxonomic_DF<-Aphia2TaxoTree(as.numeric(AphiaIDs))
Taxonomic_DF<-as.data.frame(Taxonomic_DF)
Taxonomic_DF[]<-lapply(Taxonomic_DF, as.character)

#Match the original otu name
Taxonomic_DF$Species<-str_trim(paste(SpAphia_df[,1],SpAphia_df[,2],SpAphia_df[,3],SpAphia_df[,4]))

# Fill in NAs to allow ordering taxonomically
for(i in 3:ncol(Taxonomic_DF)){
  Taxonomic_DF[is.na(Taxonomic_DF[,i]),i]<-Taxonomic_DF[is.na(Taxonomic_DF[,i]),(i-1)]
}
Taxonomic_DF[,3:ncol(Taxonomic_DF)]<-lapply(Taxonomic_DF[,3:ncol(Taxonomic_DF)], as.factor)

#### MAKE PHYLO TREE
# Taxonomic_phylo<-as.phylo(~ Phylum/Subphylum/Superclass/Class/Subclass/Infraclass/Superorder/Order/Suborder/Infraorder/Superfamily/Family/Subfamily/Infrafamily/Genus/Species, data = Taxonomic_DF[3:ncol(Taxonomic_DF)], collapse = FALSE)
# Taxonomic_phylo$edge.length<-rep(1, length(Taxonomic_phylo$edge))

# Order
Taxonomic_DF<-Taxonomic_DF[order(Taxonomic_DF$Phylum,
                                 Taxonomic_DF$Subphylum,
                                 Taxonomic_DF$Superclass,
                                 Taxonomic_DF$Class,
                                 Taxonomic_DF$Subclass,
                                 Taxonomic_DF$Infraclass,
                                 Taxonomic_DF$Superorder,
                                 Taxonomic_DF$Order,
                                 Taxonomic_DF$Suborder,
                                 Taxonomic_DF$Infraorder,
                                 Taxonomic_DF$Superfamily,
                                 Taxonomic_DF$Family,
                                 Taxonomic_DF$Subfamily,
                                 Taxonomic_DF$Infrafamily,
                                 Taxonomic_DF$Genus,
                                 Taxonomic_DF$Species),]



# Fix bad name
Taxonomic_DF[!is.na(Taxonomic_DF$Genus) & Taxonomic_DF$Genus == "Latrunculia",'ClassLevel'] <- "Genus"

temp_sp <-gsub(";", ".", Taxonomic_DF$Species)
temp_sp <-gsub("[ /]", "_", temp_sp)
temp_sp<-ifelse(str_sub(temp_sp,-2,-1) == "sp", paste0(temp_sp, "."), temp_sp)  

Taxonomic_DF_Groups<-Taxonomic_DF
for(i in 1:nrow(Taxonomic_DF_Groups)){
  if(Taxonomic_DF_Groups[i,'Genus'] %in% c("Porifera",  "Asteroidea" )){next}
  
  AddTo<-Taxonomic_DF_Groups[i, 'Species']
  
  tempdf<-Taxonomic_DF_Groups[ Taxonomic_DF_Groups[,which(colnames(Taxonomic_DF_Groups) == Taxonomic_DF_Groups[i,2])] == Taxonomic_DF_Groups[i,which(colnames(Taxonomic_DF_Groups) == Taxonomic_DF_Groups[i,2])],]
  tempdf[tempdf$Species != AddTo, paste0("AddTo",Taxonomic_DF_Groups[i,2])]<- paste(Taxonomic_DF_Groups[i,which(colnames(Taxonomic_DF_Groups) == Taxonomic_DF_Groups[i,2])])
  
  if(nrow(tempdf) < 2){next}
  
  print(paste0(c(AddTo,"<--- ", paste(tempdf$Species, collapse="; ")) , collapse= ""))  
  
  Taxonomic_DF_Groups[rownames(tempdf),paste0("AddTo",Taxonomic_DF_Groups[i,2])]<- tempdf[, paste0("AddTo",Taxonomic_DF_Groups[i,2])]
}












