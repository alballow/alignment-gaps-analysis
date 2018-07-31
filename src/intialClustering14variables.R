library(TraMineR)
library(tidyverse)
library(colorspace, RColorBrewer)
library(cluster, factoextra)
library(optmatch, RItools)
library(MatchIt)
setwd("~/Documents/my project/src/alignment-gaps-analysis")
surveyData <- read.csv("./data/phase1.csv",check.names=F,header=TRUE,sep=",")
columnNamesT<-c("partner","youngchild", "employ","school", "hhsize", "child", "move", "edu", "numcars", "avail_public", "used_public", "used_ridehail", "used_own", "used_walkbike")
alphabetT<- c(2,2,2,2,5,2,2,2,6,2,2,2,2,2)

newDFT<-map(columnNamesT, ~select(surveyData,id,age,.))

newDF2T<-map2(newDFT,columnNamesT,~spread(.x,age,.y))

newDF3T<-map(newDF2T,~select(.,2:32))

newDF4T<-map(newDF3T,~na_if(.,'.n'))
newDF4T<-map(newDF4T,~na_if(.,'.p'))
newDF4T<-map(newDF4T,~na_if(.,'.z'))
agel<-20
newDF5T<-map2(newDF4T, alphabet, ~seqdef(.x,xtstep = 5, start=agel ,right=NA, cpal=sequential_hcl(.y)))

save(newDF5T, agel, columnNames,file="temps/newDF5T")


#Faster way to do above
# newDF<-columnNames %>%
#   map(., ~select(surveyData,id,age,.)) %>%
#   map2(.,columnNames,~spread(.x,age,.y)) %>%
#   map(.,~select(.,2:32)) %>%
#   map(.,~na_if(.,'.n')) %>%
#   map(.,~na_if(.,'.p')) %>%
#   map(.,~na_if(.,'.z')) %>%
#   map(.,~seqdef(.,xtstep = 5,start=agel,right=NA))

#plot
newDF6<-map2(newDF5, columnNames, ~seqIplot(.x, sortv="from.start", main=.y))
newDF7<-map2(newDF5, columnNames, ~seqdplot(.x, main=.y))

#Optimal matching andClustering
#Optmatch package
matchit(treat~ columnNames[[2]]+columnNames[[11]], surveyData, method = "optimal", distance = "logit")






#NA 0
subcost_NA0<- seqsubm(newDF5[[5]], method = "TRATE", with.missing=T, miss.cost=0)
optimalMatchingResult0<- seqdist(newDF5[[5]], method = "OM", indel = 1, sm = subcost_NA0, with.missing=T)
clusterward0 <- agnes(optimalMatchingResult0, diss = TRUE, method = "ward")
plot(clusterward0)
clusters0<-cutree(clusterward0, k=4)
clusters0fac <- factor(clusters0, labels = paste("Type", 1:4))
seqdplot(newDF5[[5]], group = clusters0fac, border = NA, main=columnNames[[5]])
seqfplot(newDF5[[5]], group = clusters0fac, border = NA, main=columnNames[[5]])
seqIplot(newDF5[[5]], sortv="from.start", group = clusters0fac, border = NA, main=columnNames[[5]])
#NA2
subcost_NA2<- seqsubm(newDF5[[5]], method = "TRATE", with.missing=T, miss.cost=2)
optimalMatchingResult2<- seqdist(newDF5[[5]], method = "OM", indel = 1, sm = subcost_NA2, with.missing=T)
clusterward2 <- agnes(optimalMatchingResult2, diss = TRUE, method = "ward")
plot(clusterward2)
clusters2<-cutree(clusterward2, k=4)
clusters2fac <- factor(clusters2, labels = paste("Type", 1:4))
seqdplot(newDF5[[5]], group = clusters2fac, border = NA, main=columnNames[[5]])
seqfplot(newDF5[[5]], group = clusters2fac, border = NA, main=columnNames[[5]])
seqIplot(newDF5[[5]], sortv="from.start", group = clusters2fac, border = NA, main=columnNames[[5]])

#All NA2 clusters
# subcost_NA2<- map(newDF5, ~seqsubm(., method = "TRATE", with.missing=T, miss.cost=2))
# optimalMatchingResult2<- map(newDF5, ~seqdist(., method = "OM", indel = 1, sm = subcost_NA2, with.missing=T))
# clusterward2 <- agnes(optimalMatchingResult2, diss = TRUE, method = "ward")
# plot(clusterward2)
# clusters2<-cutree(clusterward2, k=4)
# clusters2fac <- factor(clusters2, labels = paste("Type", 1:4))
# map(newDF5, ~seqdplot(., group = clusters2fac, border = NA, main=columnNames))
# map(newDF5, ~seqfplot(., group = clusters2fac, border = NA, main=columnNames))

#Save to PDF
pdf("seqI.pdf",width=4,height=3,paper='special')
newDF6<-map(newDF5, ~seqIplot(., sortv="from.start", with.legend=F))
dev.off()
pdf("seqd.pdf",width=4,height=3,paper='special')
newDF7<-map(newDF5, ~seqdplot(., with.legend=F))
dev.off()
pdf("NA2clusters.pdf",width=4,height=3,paper='special')
plot(clusterward0)
seqdplot(newDF5[[5]], group = clusters2fac, border = NA, main=columnNames[[5]])
seqfplot(newDF5[[5]], group = clusters2fac, border = NA, main=columnNames[[5]])
dev.off()
pdf("NA0clusters",width=4,height=3,paper='special')
plot(clusterward0)
seqdplot(newDF5[[5]], group = clusters0fac, border = NA, main=columnNames[[5]])
seqfplot(newDF5[[5]], group = clusters0fac, border = NA, main=columnNames[[5]])
dev.off()
