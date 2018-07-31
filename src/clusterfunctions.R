library(TraMineR)
library(tidyverse)
library(colorspace, RColorBrewer)
library(cluster, factoextra)
library(WeightedCluster)
library(Rtsne)
library(ggplot2)
library(gridExtra)
library(grid)


setwd("~/Documents/my project/src/alignment-gaps-analysis")
load(file="temps/sets")

OMdistance<-function(data, numCluster, setnames, NAcost)
{
subcost_NA<- map(data, ~seqsubm(., method = "TRATE", with.missing=T, miss.cost=NAcost))
Multiplot<-seqdistmc( channels=data, method="OM", sm=subcost_NA, miss.cost=NAcost, with.missing=T)
Multiplot<-as.dist(Multiplot)
clusterward <- agnes(Multiplot, diss = TRUE, method = "ward")
#plot(clusterward)
clusters<-cutree(clusterward, k=numCluster)
clustersfac<- factor(clusters, labels = paste("Cluster", 1:numCluster))

return(Multiplot)
}

costs<-c(0, 0.4, 0.8, 1.2, 1.6, 2)
costs0.4<-c(0, 0.1, 0.2, 0.3, 0.4)

Multiplotset1<-map(costs, ~OMdistance(set1, 4, c("ifchildunder19","employ", "school","partner"), .))
Multiplotset2<-map(costs, ~OMdistance(set2, 4, c("used_walkbike","used_public", "used_own"), .))
Multiplotset1.4<-map(costs0.4, ~OMdistance(set1, 4, c("ifchildunder19","employ", "school","partner"), .))
Multiplotset2.4<-map(costs0.4, ~OMdistance(set2, 4, c("used_walkbike","used_public", "used_own"), .))
Multiplotset1short<-map(shortenedset1, ~OMdistance(., 4, c("ifchildunder19","employ", "school","partner"), 2))
Multiplotset2short<-map(shortenedset2, ~OMdistance(., 4, c("used_walkbike","used_public", "used_own"), 2))


ASWPBC<-function(Multiplot, return)
{
  clusterward <- agnes(Multiplot, diss = TRUE, method = "ward") 
  clusterRange<-as.clustrange(clusterward, Multiplot, nclusters=20)
  # plot(clusterRange, c("ASW", "PBC", "R2", "HG", "HC"))
  ASW<-data.frame(clusterRange$stats$ASW)
  PBC<-data.frame(clusterRange$stats$PBC)
  return<-eval(parse(text = return))
  return(return)
}
numberClusters<-data.frame(c(2:20))
ASWset1<-map(Multiplotset1, ~ASWPBC(., "ASW"))
ASWset2<-map(Multiplotset2, ~ASWPBC(., "ASW"))
ASWset1short<-map(Multiplotset1short, ~ASWPBC(., "ASW"))
ASWset2short<-map(Multiplotset2short, ~ASWPBC(., "ASW"))
PBCset1<-map(Multiplotset1, ~ASWPBC(., "PBC"))
PBCset2<-map(Multiplotset2, ~ASWPBC(., "PBC"))
PBCset1short<-map(Multiplotset1short, ~ASWPBC(., "PBC"))
PBCset2short<-map(Multiplotset2short, ~ASWPBC(., "PBC"))
ASWna<-bind_cols(numberClusters, ASWset1)
ASWnaset2<-bind_cols(numberClusters, ASWset2)
PBCna<-bind_cols(numberClusters, PBCset1)
PBCnaset2<-bind_cols(numberClusters, PBCset2)
ASWage1<-bind_cols(numberClusters, ASWset1short)
ASWage2<-bind_cols(numberClusters, ASWset2short)
PBCage1<-bind_cols(numberClusters, PBCset1short)
PBCage2<-bind_cols(numberClusters, PBCset2short)

clusterwardset1<-map(Multiplotset1, ~ASWPBC(., "clusterward"))
clusterwardset2<-map(Multiplotset2, ~ASWPBC(., "clusterward"))
clusterwardset1.4<-map(Multiplotset1.4, ~ASWPBC(., "clusterward"))
clusterwardset2.4<-map(Multiplotset2.4, ~ASWPBC(., "clusterward"))
clusterwardset1short<-map(Multiplotset1short, ~ASWPBC(., "clusterward"))
clusterwardset2short<-map(Multiplotset2short, ~ASWPBC(., "clusterward"))

tsne<-function(Multiplot, clusterward, perplexity, NAcost)
{
  Rtsne<-Rtsne(Multiplot, dims = 2, initial_dims = 30, perplexity = perplexity,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
  return(Rtsne)
}
perplexity<-c(30, 50, 70, 90, 100)

set1tsne<-map2(Multiplotset1,clusterwardset1,~tsne(.x, .y, 30, costs0.4))
set2tsne<-map2(Multiplotset2,clusterwardset2, ~tsne(.x, .y, 30, costs0.4))
set1tsne.4<-map2(Multiplotset1.4,clusterwardset1.4,~tsne(.x, .y, 30, costs0.4))
set2tsne.4<-map2(Multiplotset2.4,clusterwardset2.4, ~tsne(.x, .y, 30, costs0.4))
set1tsne.1<-map(perplexity, ~tsne(Multiplotset1.4[[4]], clusterwardset1.4[[4]], ., 0))
set2tsne.1<-map(perplexity, ~tsne(Multiplotset2.4[[4]], clusterwardset2.4[[4]], ., 2))
set1tsne.2<-map(perplexity, ~tsne(Multiplotset1.4[[3]], clusterwardset1.4[[3]], ., 0))
set2tsne.2<-map(perplexity, ~tsne(Multiplotset2.4[[3]], clusterwardset2.4[[3]], ., 2))
set1tsneshort<-map2(Multiplotset1short,clusterwardset1short,~tsne(.x, .y, 50, costs))
set2tsneshort<-map2(Multiplotset2short,clusterwardset2short, ~tsne(.x, .y, 50, costs))

save(set1tsneshort, set2tsneshort, set1tsne, set2tsne, set1tsne.4, set2tsne.4, set1tsne.1, set2tsne.1, set2tsne.2, set1tsne.2,
     clusterwardset1short, clusterwardset2short, clusterwardset1, clusterwardset2, clusterwardset1.4,clusterwardset2.4,
     perplexity,file="temps/t-sne")
save(OMdistance, ASWPBC, tsne, file="temps/clusterfunctions")
save(ASWna, PBCna, ASWnaset2, PBCnaset2, ASWage1, ASWage2, PBCage1, PBCage2, file="temps/ASWPBCplots")

# setnames<-c("ifchildunder19","employ", "school","partner")
# Multiplot<-seqdistmc( channels=set1, method="OM", sm=subcost_NA, miss.cost=NAcost, with.missing=T)
# Multiplot<-as.dist(Multiplot)
# clusterward <- agnes(Multiplot, diss = TRUE, method = "ward")   
# plot(clusterward)
# clusters<-cutree(clusterward, k=numCluster)
# clustersfac<- factor(clusters, labels = paste("Cluster", 1:numCluster))
# 
# seqd<-map2(set1, setnames,~seqdplot(.x, group = clustersfac, border = NA, main=paste("NA", NAcost, .y), cols=numCluster))
# seqf<-map2(set1, setnames,~seqfplot(.x, group = clustersfac, border = NA, main=paste("NA", NAcost, .y), cols=numCluster))
# seqI<-map2(set1, setnames,~seqIplot(.x, sortv="from.start", group = clustersfac, border = NA, main=paste("NA", NAcost, .y), cols=numCluster))
# 
# grid.arrange(seqd[[1]], seqd[[2]], seqd[[3]], seqd[[4]],
#              seqf[[1]], seqf[[2]], seqf[[3]], seqf[[4]],
#              seqI[[1]], seqI[[2]], seqI[[3]], seqI[[4]], ncol=4)





