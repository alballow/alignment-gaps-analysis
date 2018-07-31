library(TraMineR)
library(tidyverse)
library(colorspace, RColorBrewer)
library(cluster, factoextra)
library(WeightedCluster)

setwd("~/Documents/my project/src/alignment-gaps-analysis")
load(file="newDF5")

#Optimal matching andClustering
  #NA0 clusters
    #Combined
    subcost_NA0<- map(newDF5, ~seqsubm(., method = "TRATE", with.missing=T, miss.cost=0))
    Multiplot0<-seqdistmc( channels=newDF5, method="OM", sm=subcost_NA0, miss.cost=0, with.missing=T)
    Multiplot0<-as.dist(Multiplot0)
    clusterward0 <- agnes(Multiplot0, diss = TRUE, method = "ward")   
    plot(clusterward0)
    clusters0<-cutree(clusterward0, k=5)
    clusters0fac<- factor(clusters0, labels = paste("Cluster", 1:5))
    map2(newDF5, columnNames, ~seqdplot(.x, group = clusters0fac, border = NA, main=paste("NA0", .y)))
    map2(newDF5, columnNames,~seqfplot(.x, group = clusters0fac, border = NA, main=paste("NA0", .y)))
    map2(newDF5, columnNames,~seqIplot(.x, sortv="from.start", group = clusters0fac, border = NA, main=paste("NA0", .y)))
    #nominal
    subcost_NA0nominal<-seqsubm(newDF5[[5]], method = "TRATE", with.missing=T, miss.cost=0)
    Multiplot0nominal<-seqdist(newDF5[[5]], method="OM", sm=subcost_NA0nominal, with.missing=T)
    clusterward0nominal <- agnes(Multiplot0nominal, diss = TRUE, method = "ward")
    clusters0nominal<-cutree(clusterward0nominal, k=5)
    #binary
    subcost_NA0binary<- map(newDF5binary, ~seqsubm(., method = "TRATE", with.missing=T, miss.cost=0))
    Multiplot0binary<-seqdistmc( channels=newDF5binary, method="OM", sm=subcost_NA0binary, miss.cost=0, with.missing=T)
    clusterward0binary <- agnes(Multiplot0binary, diss = TRUE, method = "ward")
    clusters0binary<-cutree(clusterward0binary, k=5)

  #NA2 clusters
    #Combined
    subcost_NA2<- map(newDF5, ~seqsubm(., method = "TRATE", with.missing=T, miss.cost=2))
    Multiplot2<-seqdistmc( channels=newDF5, method="OM", sm=subcost_NA2, miss.cost=2, with.missing=T)
    Multiplot2<-as.dist(Multiplot2)
    clusterward2 <- agnes(Multiplot2, diss = TRUE, method = "ward")   
    plot(clusterward2)
    clusters2<-cutree(clusterward2, k=5)
    clusters2fac<- factor(clusters2, labels = paste("Cluster", 1:5))
    map2(newDF5, columnNames, ~seqdplot(.x, group = clusters2fac, border = NA, main=paste("NA2", .y)))
    map2(newDF5, columnNames,~seqfplot(.x, group = clusters2fac, border = NA, main=paste("NA2", .y)))
    map2(newDF5, columnNames,~seqIplot(.x, sortv="from.start", group = clusters2fac, border = NA, main=paste("NA2", .y)))
    #Nominal
    subcost_NA2nominal<-seqsubm(newDF5[[5]], method = "TRATE", with.missing=T, miss.cost=2)
    Multiplot2nominal<-seqdist(newDF5[[5]], method="OM", sm=subcost_NA2nominal, with.missing=T)
    clusterward2nominal <- agnes(Multiplot2nominal, diss = TRUE, method = "ward")
    clusters2nominal<-cutree(clusterward2nominal, k=5)
    #binary
    subcost_NA2binary<- map(newDF5binary, ~seqsubm(., method = "TRATE", with.missing=T, miss.cost=2))
    Multiplot2binary<-seqdistmc( channels=newDF5binary, method="OM", sm=subcost_NA2binary, miss.cost=2, with.missing=T)
    clusterward2binary <- agnes(Multiplot2binary, diss = TRUE, method = "ward")
    clusters2binary<-cutree(clusterward2binary, k=5)
    
    numberClusters<-data.frame(c(2:20))

save(numberClusters, Multiplot0, clusters0, clusterward0, Multiplot2, clusters2, clusterward2, 
     Multiplot0binary, clusters0binary, clusterward0binary, Multiplot2binary, clusters2binary, clusterward2binary,
     Multiplot0nominal, clusters0nominal, clusterward0nominal, Multiplot2nominal, clusters2nominal, clusterward2nominal, 
     file="temps/Clusterinfo")

#Save to PDF
pdf("plots/NA2clusters.pdf",width=4,height=4,paper='special')
par(cex.main=.5, cex.lab=.5, cex.axis=.5, xaxs="r", yaxs="r", lwd=.5, mgp=c(0,0,0))
plot(clusterward2)
map2(newDF5, columnNames, ~seqdplot(.x, group = clusters2fac, border = NA, main=list(paste("NA2", .y), cex=.5)))
map2(newDF5, columnNames,~seqfplot(.x, group = clusters2fac, border = NA, main=paste("NA2", .y)))
map2(newDF5, columnNames,~seqIplot(.x, sortv="from.start", group = clusters2fac, border = NA, main=paste("NA2", .y)))
dev.off()
pdf("plots/NA0clusters.pdf",width=4,height=4,paper='special')
par(cex.main=.5, cex.lab=.5, xaxs=.5, yaxs=.5)
plot(clusterward0)
map2(newDF5, columnNames, ~seqdplot(.x, group = clusters0fac, border = NA, main=paste("NA0", .y),with.legend=F))
map2(newDF5, columnNames,~seqfplot(.x, group = clusters0fac, border = NA, main=paste("NA0", .y),with.legend=F))
map2(newDF5, columnNames,~seqIplot(.x, sortv="from.start", group = clusters0fac, border = NA, main=paste("NA0", .y),with.legend=F))
dev.off()