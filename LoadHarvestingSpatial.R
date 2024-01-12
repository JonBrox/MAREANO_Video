FishingEffort<-read.csv(paste0(PathInputData,"effort_fish.csv"), sep = ",")
ShrimpingEffort<-read.csv(paste0(PathInputData,"effort_shrimp.csv"), sep = ",")

FishingEffort[log(FishingEffort$Val_meankwHour+1)<6,'RelativeEffort']<-0
FishingEffort[log(FishingEffort$Val_meankwHour+1)>=6 & log(FishingEffort$Val_meankwHour+1) <8.7,'RelativeEffort']<-1
FishingEffort[log(FishingEffort$Val_meankwHour+1)>=8.7 & log(FishingEffort$Val_meankwHour+1) < median(log(FishingEffort$Val_meankwHour+1)[FishingEffort$Val_meankwHour!=0]),'RelativeEffort']<-2
FishingEffort[log(FishingEffort$Val_meankwHour+1)>=median(log(FishingEffort$Val_meankwHour+1)[FishingEffort$Val_meankwHour!=0]) & log(FishingEffort$Val_meankwHour+1) < 10,'RelativeEffort']<-3
FishingEffort[log(FishingEffort$Val_meankwHour+1)>=10 ,'RelativeEffort']<-4

hist(log(FishingEffort$Val_meankwHour[FishingEffort$Val_meankwHour!= 0 ]), breaks =100)
abline(v=5, col="blue")
abline(v=8.7, col="yellow")
abline(v= median(log(FishingEffort$Val_meankwHour+1)[FishingEffort$Val_meankwHour!=0]), col="orange")
abline(v=10, col="red")

table(FishingEffort$RelativeEffort)

ShrimpingEffort[log(ShrimpingEffort$Val_meankwHour+1)<5,'RelativeEffort']<-0
ShrimpingEffort[log(ShrimpingEffort$Val_meankwHour+1)>=5 & log(ShrimpingEffort$Val_meankwHour+1) <7.9,'RelativeEffort']<-1
ShrimpingEffort[log(ShrimpingEffort$Val_meankwHour+1)>=7.9 & log(ShrimpingEffort$Val_meankwHour+1) < median(log(ShrimpingEffort$Val_meankwHour+1)[ShrimpingEffort$Val_meankwHour!=0]),'RelativeEffort']<-2
ShrimpingEffort[log(ShrimpingEffort$Val_meankwHour+1)>=median(log(ShrimpingEffort$Val_meankwHour+1)[ShrimpingEffort$Val_meankwHour!=0]) & log(ShrimpingEffort$Val_meankwHour+1) <8.9,'RelativeEffort']<-3
ShrimpingEffort[log(ShrimpingEffort$Val_meankwHour+1)>=8.9 ,'RelativeEffort']<-4

hist(log(ShrimpingEffort$Val_meankwHour[ShrimpingEffort$Val_meankwHour!= 0 ]), breaks =100)
abline(v=5, col="blue")
abline(v=7.9, col="yellow")
abline(v=median(log(ShrimpingEffort$Val_meankwHour+1)[ShrimpingEffort$Val_meankwHour!=0]), col="orange")
abline(v=8.9, col="red")

table(ShrimpingEffort$RelativeEffort)

FishingEffort$AllRelativeEffort<-pmax(FishingEffort$RelativeEffort, ShrimpingEffort$RelativeEffort)

rHarvestEffort<-rasterFromXYZ(FishingEffort[,c(2,1,6)]) 

proj4string(rHarvestEffort)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
rHarvestEffort <- projectRaster(rHarvestEffort, crs = "+proj=utm +ellps=WGS84 +zone=33 +datum=WGS84")

rHarvestEffort@data@values<-round(rHarvestEffort@data@values)
plot(rHarvestEffort)


