library(TraMineR)
library(tidyverse)
library(colorspace, RColorBrewer)
library(cluster, factoextra, WeightedClusters)
library(optmatch, RItools)
library(MatchIt)
setwd("~/Documents/my project/src/alignment-gaps-analysis")
surveyData <- read.csv("./data/phase1.csv",check.names=F,header=TRUE,sep=",")
columnNames<-c("partner","youngchild", "employ","school", "hhsize", "child", "move", "edu", "numcars", "avail_public", "used_public", "used_ridehail", "used_own", "used_walkbike")
alphabet<- c(2,2,2,2,5,2,2,2,6,2,2,2,2,2)

data<-select(surveyData, id, age, youngchild)
data8<-select(surveyData, id, age, used_public)
data2<-spread(data8,age, used_public)
data3<-spread(data,age, youngchild)

data4<-bind_rows(data2, data3)
data5<-select(data4, 2:31)

data6<-na_if(data5,'.n')
data6<-na_if(data6,'.p')
data6<-na_if(data6,'.z')
agel<-20
data7<-seqdef(data6,xtstep = 5, start=agel ,right=NA)

subcost_NA0<- seqsubm(data7, method = "TRATE", with.missing=T, miss.cost=0)
optimalMatchingResult0<- seqdist(data7, method = "OM", indel = 1, sm = subcost_NA0, with.missing=T, full.matrix=T)
optimalMatchingSelectValues<-optimalMatchingResult0[913:182, 1:912]
diagonalresults<-diag(optimalMatchingSelectValues)
summary(diagonalresults)
diagonalresults[diagonalresults!=0]
data2_s<-data2[diagonalresults!=0,]
data3_s<-data3[diagonalresults!=0,]
data4_s<-select(data2_s, 2:31)
data5_s<-select(data3_s, 2:31)
data6_s<-na_if(data4_s,'.n')
data6_s<-na_if(data6_s,'.p')
data6_s<-na_if(data6_s,'.z')
data7_s<-na_if(data5_s,'.n')
data7_s<-na_if(data7_s,'.p')
data7_s<-na_if(data7_s,'.z')
data8_s<-data6_s[rowSums(is.na(data7_s)) != ncol(data7_s), ]
data9_s<-data7_s[rowSums(is.na(data7_s)) != ncol(data7_s), ]
data2_seqdef<-seqdef(data8_s,xtstep = 5, start=agel ,right=NA, cpal=rainbow_hcl(2))
data3_seqdef<-seqdef(data9_s,xtstep = 5, start=agel ,right=NA, cpal=rainbow_hcl(2))
seqdplot(data2_seqdef, main="used-public")
seqdplot(data3_seqdef, main="youngchild")
seqiplot(data2_seqdef, sortv="from.start", main="used-public")
seqiplot(data3_seqdef,sortv="from.start",  main="youngchild")
seqIplot(data2_seqdef, main="used-public")
seqIplot(data3_seqdef,  main="youngchild")

subcost_NA0<- seqsubm(data3_seqdef, method = "TRATE", with.missing=T, miss.cost=0)
optimalMatchingResult0<- seqdist(data3_seqdef, method = "OM", indel = 1, sm = subcost_NA0, with.missing=T, full.matrix=T)
clusterward0 <- agnes(optimalMatchingResult0, method = "ward")
plot(clusterward0)
clusters0<-cutree(clusterward0, k=4)
clusters0fac <- factor(clusters0, labels = paste("Cluster", 1:4))
seqdplot(data3_seqdef, group = clusters0fac, border = NA, main=paste("NA0", "youngchild v. used_public"))
seqfplot(data3_seqdef, group = clusters0fac, border = NA, main=paste("NA0", "youngchild v. used_public"))
seqIplot(data3_seqdef, sortv="from.start", group = clusters0fac, border = NA, main=paste("NA0", "youngchild"))
seqiplot(data3_seqdef, idxs=1:21, group = clusters0fac, border = NA, main=paste("NA0", "youngchild" ))


subcost_NA0<- seqsubm(data3_seqdef, method = "TRATE", with.missing=T, miss.cost=0)
optimalMatchingResult0<- seqdist(data3_seqdef, method = "OM", indel = 1, sm = subcost_NA0, with.missing=T, full.matrix=T)
clusterward0 <- agnes(optimalMatchingResult0, method = "ward")
plot(clusterward0)
clusters0<-cutree(clusterward0, k=4)
clusters0fac <- factor(clusters0, labels = paste("Cluster", 1:4))
seqdplot(data2_seqdef, group = clusters0fac, border = NA, main=paste("NA0", "youngchild v. used_public"))
seqfplot(data2_seqdef, group = clusters0fac, border = NA, main=paste("NA0", "youngchild v. used_public"))
seqIplot(data2_seqdef, sortv="from.start", group = clusters0fac, border = NA, main=paste("NA0", " used_public"))
seqiplot(data2_seqdef, idxs=1:21, group = clusters0fac, border = NA, main=paste( "used_public"))

pdf("plots/Comparison of used_public and youngchild",width=4,height=4,paper='special')
seqIplot(data2_seqdef, sortv="from.start", group = clusters0fac, border = NA, main=paste("NA0", " used_public"))
seqiplot(data2_seqdef, idxs=1:21, group = clusters0fac, border = NA, main=paste( "used_public"))
seqIplot(data3_seqdef, sortv="from.start", group = clusters0fac, border = NA, main=paste("NA0", "youngchild"))
seqiplot(data3_seqdef, idxs=1:21, group = clusters0fac, border = NA, main=paste("NA0", "youngchild" ))
dev.off()




