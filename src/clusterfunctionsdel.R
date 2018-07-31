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
load(file="temps/setsdel")
load(file="temps/clusterfunctions")

OMdistancenorm<-function(data, numCluster, setnames, NAcost, norm)
{
  subcost_NA<- map(data, ~seqsubm(., method = "TRATE", with.missing=T, miss.cost=NAcost))
  Multiplot<-seqdistmc( channels=data, method="OM", sm=subcost_NA, miss.cost=NAcost, with.missing=T, norm=norm)
  Multiplot<-as.dist(Multiplot)
  clusterward <- agnes(Multiplot, diss = TRUE, method = "ward")   
  #plot(clusterward)
  clusters<-cutree(clusterward, k=numCluster)
  # clustersfac<- factor(clusters, labels = paste("Cluster", 1:numCluster))
  
  return(Multiplot)
}
  
costs0.4<-c(0, 0.1, 0.2, 0.3, 0.4)
costs<-c(2, 1.6, 1.2, .8, .4, 0 )


Multiplotset1shortdel<-map(shortenedset1del, ~OMdistance(., 4, c("ifchildunder19","employ", "school","partner"), 2))
Multiplotset2shortdel<-map(shortenedset2del, ~OMdistance(., 4, c("used_walkbike","used_public", "used_own"), 2))
# Multiplotset1shortdel40<-map(shortenedset1del40, ~OMdistance(., 4, c("ifchildunder19","employ", "school","partner"), 2))
# Multiplotset2shortdel40<-map(shortenedset2del40, ~OMdistance(., 4, c("used_walkbike","used_public", "used_own"), 2))
Multiplotset1shortdelnormlength<-map(shortenedset1delnorm, ~OMdistancenorm(., 4, c("ifchildunder19","employ", "school","partner"), 2, "maxlength"))
Multiplotset2shortdelnormlength<-map(shortenedset2delnorm, ~OMdistancenorm(., 4, c("used_walkbike","used_public", "used_own"), 2, "maxlength"))
# Multiplotset1shortdelnormlength40<-map(shortenedset1del40, ~OMdistancenorm(., 4, c("ifchildunder19","employ", "school","partner"), 2, "maxlength"))
# Multiplotset2shortdelnormlength40<-map(shortenedset2del40, ~OMdistancenorm(., 4, c("used_walkbike","used_public", "used_own"), 2, "maxlength"))
# Multiplotset1shortdelnormdist<-map(shortenedset1delnorm, ~OMdistancenorm(., 4, c("ifchildunder19","employ", "school","partner"), 2, "maxdist"))
# Multiplotset2shortdelnormdist<-map(shortenedset2delnorm, ~OMdistancenorm(., 4, c("used_walkbike","used_public", "used_own"), 2, "maxdist"))
# Multiplotset1shortdelnormmean<-map(shortenedset1delnorm, ~OMdistancenorm(., 4, c("ifchildunder19","employ", "school","partner"), 2, "gmean"))
# Multiplotset2shortdelnormmean<-map(shortenedset2delnorm, ~OMdistancenorm(., 4, c("used_walkbike","used_public", "used_own"), 2, "gmean"))
# Multiplotset1shortdelnormy<-map(shortenedset1delnorm, ~OMdistancenorm(., 4, c("ifchildunder19","employ", "school","partner"), 2, "YujianBo"))
# Multiplotset2shortdelnormy<-map(shortenedset2delnorm, ~OMdistancenorm(., 4, c("used_walkbike","used_public", "used_own"), 2, "YujianBo"))

ASWPBC<-function(Multiplot, return)
{
  clusterward <- agnes(Multiplot, diss = TRUE, method = "ward", return) 
  clusterRange<-as.clustrange(clusterward, Multiplot, nclusters=20)
  # plot(clusterRange, c("ASW", "PBC", "R2", "HG", "HC"))
  ASW<-data.frame(clusterRange$stats$ASW)
  PBC<-data.frame(clusterRange$stats$PBC)
  R2<-data.frame(clusterRange$stats$R2)
  HG<-data.frame(clusterRange$stats$HG)
  HC<-data.frame(clusterRange$stats$HC)
  return<-eval(parse(text = return))
  return(return)
}
numberClusters<-data.frame(c(2:20))
ASWset1short<-ASWPBC(Multiplotset1shortdel[[3]], "ASW")
ASWset2short<-ASWPBC(Multiplotset2shortdel[[3]], "ASW")
PBCset1short<-ASWPBC(Multiplotset1shortdel[[3]], "PBC")
PBCset2short<-ASWPBC(Multiplotset2shortdel[[3]], "PBC")
R2set1short<-ASWPBC(Multiplotset1shortdel[[3]], "R2")
R2set2short<-ASWPBC(Multiplotset2shortdel[[3]], "R2")
HGset1short<-ASWPBC(Multiplotset1shortdel[[3]], "HG")
HGset2short<-ASWPBC(Multiplotset2shortdel[[3]], "HG")
HCset1short<-ASWPBC(Multiplotset1shortdel[[3]], "HC")
HCset2short<-ASWPBC(Multiplotset2shortdel[[3]], "HC")
ASWPBCR2HGHCset1<-bind_cols(numberClusters, ASWset1short, PBCset1short, HGset1short, HCset1short)
ASWPBCR2HGHCset2<-bind_cols(numberClusters, ASWset2short, PBCset2short, HGset2short, HCset2short)


clusterwardset1shortdel<-map(Multiplotset1shortdel, ~ASWPBC(., "clusterward"))
clusterwardset2shortdel<-map(Multiplotset2shortdel, ~ASWPBC(., "clusterward"))
# clusterwardset1shortdel40<-map(Multiplotset1shortdel40, ~ASWPBC(., "clusterward"))
# clusterwardset2shortdel40<-map(Multiplotset2shortdel40, ~ASWPBC(., "clusterward"))

clusterwardset1shortdelnormlength<-map(Multiplotset1shortdelnormlength, ~agnes(., diss = TRUE, method = "ward"))
clusterwardset2shortdelnormlength<-map(Multiplotset2shortdelnormlength, ~agnes(., diss = TRUE, method = "ward"))
# clusterwardset1shortdelnormlength40<-map(Multiplotset1shortdelnormlength40, ~agnes(., diss = TRUE, method = "ward"))
# clusterwardset2shortdelnormlength40<-map(Multiplotset2shortdelnormlength40, ~agnes(., diss = TRUE, method = "ward"))
# clusterwardset1shortdelnormdist<-map(Multiplotset1shortdelnormdist, ~agnes(., diss = TRUE, method = "ward"))
# clusterwardset2shortdelnormdist<-map(Multiplotset2shortdelnormdist, ~agnes(., diss = TRUE, method = "ward"))
# clusterwardset1shortdelnormmean<-map(Multiplotset1shortdelnormmean, ~agnes(., diss = TRUE, method = "ward"))
# clusterwardset2shortdelnormmean<-map(Multiplotset2shortdelnormmean, ~agnes(., diss = TRUE, method = "ward"))
# clusterwardset1shortdelnormy<-map(Multiplotset1shortdelnormy, ~agnes(., diss = TRUE, method = "ward"))
# clusterwardset2shortdelnormy<-map(Multiplotset2shortdelnormy, ~agnes(., diss = TRUE, method = "ward"))


perplexity<-c(30, 50, 70, 90, 100)

set1tsneshortdel<-map2(Multiplotset1shortdel,clusterwardset1shortdel, ~tsne(.x, .y, 50, costs))
set2tsneshortdel<-map2(Multiplotset2shortdel,clusterwardset2shortdel, ~tsne(.x, .y, 50, costs))
# set1tsneshortdel40<-map2(Multiplotset1shortdel40,clusterwardset1shortdel40, ~tsne(.x, .y, 50, costs))
# set2tsneshortdel40<-map2(Multiplotset2shortdel40,clusterwardset2shortdel40, ~tsne(.x, .y, 50, costs))
set1tsneshortdelnormlength<-map2(Multiplotset1shortdelnormlength,clusterwardset1shortdelnormlength, ~tsne(.x, .y, 50, costs))
set2tsneshortdelnormlength<-map2(Multiplotset2shortdelnormlength,clusterwardset2shortdelnormlength, ~tsne(.x, .y, 50, costs))
# set1tsneshortdelnormlength40<-map2(Multiplotset1shortdelnormlength40,clusterwardset1shortdelnormlength40, ~tsne(.x, .y, 50, costs))
# set2tsneshortdelnormlength40<-map2(Multiplotset2shortdelnormlength40,clusterwardset2shortdelnormlength40, ~tsne(.x, .y, 50, costs))
# set1tsneshortdelnormdist<-map2(Multiplotset1shortdelnormdist,clusterwardset1shortdelnormdist, ~tsne(.x, .y, 50, costs))
# set2tsneshortdelnormdist<-map2(Multiplotset2shortdelnormdist,clusterwardset2shortdelnormdist, ~tsne(.x, .y, 50, costs))
# set1tsneshortdelnormmean<-map2(Multiplotset1shortdelnormmean,clusterwardset1shortdelnormmean, ~tsne(.x, .y, 50, costs))
# set2tsneshortdelnormmean<-map2(Multiplotset2shortdelnormmean,clusterwardset2shortdelnormmean, ~tsne(.x, .y, 50, costs))
# set1tsneshortdelnormy<-map2(Multiplotset1shortdelnormy,clusterwardset1shortdelnormy, ~tsne(.x, .y, 50, costs))
# set2tsneshortdelnormy<-map2(Multiplotset2shortdelnormy,clusterwardset2shortdelnormy, ~tsne(.x, .y, 50, costs))
set1tsneperpdel<-map(perplexity, ~tsne(Multiplotset1shortdel[[3]], clusterwardset1shortdel[[3]], ., costs))
set2tsneperpdel<-map(perplexity, ~tsne(Multiplotset2shortdel[[3]], clusterwardset2shortdel[[3]], ., costs))
set1tsneperpdelnorm<-map(perplexity, ~tsne(Multiplotset1shortdelnormlength[[3]], clusterwardset1shortdelnormlength[[3]], ., costs))
set2tsneperpdelnorm<-map(perplexity, ~tsne(Multiplotset2shortdelnormlength[[3]], clusterwardset2shortdelnormlength[[3]], ., costs))

save(set1tsneshortdel, set2tsneshortdel, set1tsneshortdelnormlength, set2tsneshortdelnormlength,
     set1tsneperpdel, set2tsneperpdel, set1tsneperpdelnorm, set2tsneperpdelnorm,
     clusterwardset1shortdel, clusterwardset2shortdel,clusterwardset1shortdelnormlength, clusterwardset2shortdelnormlength,
     #  set1tsneshortdelnormdist, set2tsneshortdelnormdist,
     # set1tsneshortdelnormmean, set2tsneshortdelnormmean, set1tsneshortdelnormy, set2tsneshortdelnormy,
     file="temps/t-snedel")
save(Multiplotset1shortdel, Multiplotset2shortdel, Multiplotset1shortdelnormlength,
     Multiplotset2shortdelnormlength, file="temps/Multiplots")
save(ASWPBCR2HGHCset1, ASWPBCR2HGHCset2, file="temps/ASWPBCplotsdel")


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





