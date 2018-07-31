library(TraMineR)
library(tidyverse)
library(colorspace, RColorBrewer)
library(gridExtra)
library(cluster, factoextra)
library(WeightedCluster)
library(reshape2)
library(ggplot2)
library(Rtsne)
library(tsne)
library(FactoMineR)
setwd("~/Documents/my project/src/alignment-gaps-analysis")
surveyData <- read.csv("./data/phase1.csv",check.names=F,header=TRUE,sep=",")
columnNames<-c("partner","youngchild", "employ","school", "hhsize")
columnNamesbinary<-c("partner","youngchild", "employ","school")
alphabet<- c(2,2,2,2,5)
agel<-20

DF<-filter(reduced35, id %in% ids$id)
newDF<-map(columnNames, ~select(new_data,id,age,.))
newDF2<-map2(newDF,columnNames,~spread(.x,age,.y))
newDF3<-map(newDF2,~select(.,2:32))
newDF4<-map(newDF3,~na_if(.,'.n'))
newDF4<-map(newDF4,~na_if(.,'.p'))
newDF4<-map(newDF4,~na_if(.,'.z'))
newDF5<-map2(newDF4,alphabet, ~seqdef(.x,xtstep = 5, start=agel ,right=NA, cpal=sequential_hcl(.y)))
newDFbinary<-map(columnNamesbinary, ~select(surveyData,id,age,.))
newDF2binary<-map2(newDFbinary,columnNamesbinary,~spread(.x,age,.y))
newDF3binary<-map(newDF2binary,~select(.,2:32))
newDF4binary<-map(newDF3binary,~na_if(.,'.n'))
newDF4binary<-map(newDF4binary,~na_if(.,'.p'))
newDF4binary<-map(newDF4binary,~na_if(.,'.z'))
newDF5binary<-map(newDF4binary, ~seqdef(.x,xtstep = 5, start=agel ,right=NA, cpal=sequential_hcl(2)))

save(newDF5, newDF5binary, agel, columnNames,file="temps/newDF5")



id<-select(surveyData, id, hhsize, age)
id<-filter(id, age==20)
id<-filter(id, hhsize %in% c(1:5))
ids<-select(id, id)

id %in% ids$id


# #plot
# seqIplots<-map2(newDF5, columnNames, ~seqIplot(.x, sortv="from.start", main= .y))
# seqdplots<-map2(newDF5, columnNames, ~seqdplot(.x, main=.y))
# 
# #Optimal matching andClustering
#   #All NA0 clusters
# subcost_NA0<- map(newDF5, ~seqsubm(., method = "TRATE", with.missing=T, miss.cost=0))
# Multiplot0<-seqdistmc( channels=newDF5, method="OM", sm=subcost_NA0, miss.cost=0, with.missing=T)
# Multiplot0<-as.dist(Multiplot0)
# clusterward0 <- agnes(Multiplot0, diss = TRUE, method = "ward")   
# plot(clusterward0)
# clusters0<-cutree(clusterward0, k=5)
# clusters0fac<- factor(clusters0, labels = paste("Cluster", 1:5))
# map2(newDF5, columnNames, ~seqdplot(.x, group = clusters0fac, border = NA, main=paste("NA0", .y)))
# map2(newDF5, columnNames,~seqfplot(.x, group = clusters0fac, border = NA, main=paste("NA0", .y)))
# map2(newDF5, columnNames,~seqIplot(.x, sortv="from.start", group = clusters0fac, border = NA, main=paste("NA0", .y)))
# clusterRange0<-as.clustrange(clusterward0, Multiplot0, nclusters=20)
# plot(clusterRange0, c("ASW", "PBC"))
# ASW0<-data.frame(clusterRange0$stats$ASW)
# PBC0<-data.frame(clusterRange0$stats$PBC)
# 
#   #All NA2 clusters
# subcost_NA2<- map(newDF5, ~seqsubm(., method = "TRATE", with.missing=T, miss.cost=2))
# Multiplot2<-seqdistmc( channels=newDF5, method="OM", sm=subcost_NA0, miss.cost=2, with.missing=T)
# Multiplot2<-as.dist(Multiplot2)
# clusterward2 <- agnes(Multiplot2, diss = TRUE, method = "ward")   
# plot(clusterward2)
# clusters2<-cutree(clusterward2, k=5)
# clusters2fac<- factor(clusters2, labels = paste("Cluster", 1:5))
# map2(newDF5, columnNames, ~seqdplot(.x, group = clusters2fac, border = NA, main=paste("NA2", .y)))
# map2(newDF5, columnNames,~seqfplot(.x, group = clusters2fac, border = NA, main=paste("NA2", .y)))
# map2(newDF5, columnNames,~seqIplot(.x, sortv="from.start", group = clusters2fac, border = NA, main=paste("NA2", .y)))
# clusterRange2<-as.clustrange(clusterward2, Multiplot2, nclusters=20)
# plot(clusterRange2, c("ASW", "PBC"))
# ASW2<-data.frame(clusterRange2$stats$ASW)
# PBC2<-data.frame(clusterRange2$stats$PBC)
# 
# #Determining the proper number of clusters
# numberClusters<-data.frame(c(2:20))
# ASWcombined<-bind_cols( ASW0, ASW2)
# PBCcombined<-bind_cols(PBC0,PBC2)
#   #nominal
#     #NA 0
# subcost_NA0nominal<-seqsubm(newDF5[[5]], method = "TRATE", with.missing=T, miss.cost=0)
# Multiplot0nominal<-seqdist(newDF5[[5]], method="OM", sm=subcost_NA0nominal, with.missing=T)
# clusterward0nominal <- agnes(Multiplot0nominal, diss = TRUE, method = "ward")
# clusters0nominal<-cutree(clusterward0nominal, k=5)
# clusterRange0nominal<-as.clustrange(clusterward0nominal, Multiplot0nominal, nclusters=20)
# ASW0nominal<-data.frame(clusterRange0nominal$stats$ASW)
# PBC0nominal<-data.frame(clusterRange0nominal$stats$PBC)
#     #NA2
# subcost_NA2nominal<-seqsubm(newDF5[[5]], method = "TRATE", with.missing=T, miss.cost=2)
# Multiplot2nominal<-seqdist(newDF5[[5]], method="OM", sm=subcost_NA2nominal, with.missing=T)
# clusterward2nominal <- agnes(Multiplot2nominal, diss = TRUE, method = "ward")
# clusters2nominal<-cutree(clusterward2nominal, k=5)
# clusterRange2nominal<-as.clustrange(clusterward2nominal, Multiplot2nominal, nclusters=20)
# ASW2nominal<-data.frame(clusterRange2nominal$stats$ASW)
# PBC2nominal<-data.frame(clusterRange2nominal$stats$PBC)
# 
#   #binary
#     #NA 0
# subcost_NA0binary<- map(newDF5binary, ~seqsubm(., method = "TRATE", with.missing=T, miss.cost=0))
# Multiplot0binary<-seqdistmc( channels=newDF5binary, method="OM", sm=subcost_NA0binary, miss.cost=0, with.missing=T)
# clusterward0binary <- agnes(Multiplot0binary, diss = TRUE, method = "ward")
# clusters0binary<-cutree(clusterward0binary, k=5)
# clusterRange0binary<-as.clustrange(clusterward0binary, Multiplot0binary, nclusters=20)
# ASW0binary<-data.frame(clusterRange0binary$stats$ASW)
# PBC0binary<-data.frame(clusterRange0binary$stats$PBC)
#     #NA 2
# subcost_NA2binary<- map(newDF5binary, ~seqsubm(., method = "TRATE", with.missing=T, miss.cost=2))
# Multiplot2binary<-seqdistmc( channels=newDF5binary, method="OM", sm=subcost_NA2binary, miss.cost=2, with.missing=T)
# clusterward2binary <- agnes(Multiplot2binary, diss = TRUE, method = "ward")
# clusters2binary<-cutree(clusterward2binary, k=5)
# clusterRange2binary<-as.clustrange(clusterward2binary, Multiplot2binary, nclusters=20)
# ASW2binary<-data.frame(clusterRange2binary$stats$ASW)
# PBC2binary<-data.frame(clusterRange2binary$stats$PBC)
# #plotting
#   #ASW
# ASWnominal<-bind_cols(ASW0nominal, ASW2nominal)
# ASWbinary<-bind_cols(ASW0binary, ASW2binary)
# ASW<-bind_cols(numberClusters, ASWnominal,  ASWbinary, ASWcombined)
# colnames(ASW)<-c("numberClusters", "ASWn0", "ASWn2", "ASWb0", "ASWb2", "ASWc0", "ASWc2")
# ASW<-melt(ASW, id.vars="numberClusters")
# ASW$DT<-factor(rep(0:2, each=38))
# ASW$NAc<-factor(rep(0:1, each=19))
# plot<-ggplot(data=ASW,aes(x=numberClusters,y=value,group=variable))
# plot<-plot+geom_point( aes(group=variable,color=DT),size = 4)
# plot<-plot+geom_line(aes(linetype=NAc,color=DT), size=1.5)
# plot<-plot + theme(axis.text.x = element_text(angle = 00, hjust = 1, size=13,color="black",face='bold'))
# plot<-plot + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13,color="black",face='bold'))
# plot<-plot+scale_color_discrete(name="Data Type", breaks=c("0", "1", "2"),labels=c("Combined", "Binary", "Nominal"))
# plot<-plot+scale_linetype_discrete(name="NA cost", breaks=c("0", "1"),labels=c("NA cost 0", "NA cost 2"))
# plot<-plot+coord_cartesian(xlim =c(1, 20),ylim=c(0.1,0.9)) 
# plot<-plot + scale_y_continuous(breaks=seq(0.1,0.8,0.1))
# plot<-plot+xlab("Number of Clusters")+ylab("ASW")
# plot<-plot + theme(axis.title.x = element_text(face='bold', size=13))
# plot<-plot + theme(axis.title.y = element_text(face='bold', size=13))
# ASWplot<-plot + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
# print(ASWplot)
#   #PBC
# PBCnominal<-bind_cols(PBC0nominal,PBC2nominal)
# PBCbinary<-bind_cols(PBC0binary,PBC2binary)
# PBC<-bind_cols(numberClusters, PBCnominal, PBCbinary, PBCcombined)
# colnames(PBC)<-c("numberClusters", "PBCn0", "PBCn2", "PBCb0", "PBCb2", "PBCc0", "PBCc2")
# PBC<-melt(PBC, id.vars="numberClusters")
# PBC$DT<-factor(rep(0:2, each=38))
# PBC$NAc<-factor(rep(0:1, each=19))
# plot<-ggplot(data=PBC,aes(x=numberClusters,y=value,group=variable))
# plot<-plot+geom_point( aes(group=variable,color=DT),size = 4)
# plot<-plot+geom_line(aes(linetype=NAc,color=DT), size=1.5)
# plot<-plot + theme(axis.text.x = element_text(angle = 00, hjust = 1, size=13,color="black",face='bold'))
# plot<-plot + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13,color="black",face='bold'))
# plot<-plot+scale_color_discrete(name="Data Type", breaks=c("0", "1", "2"),labels=c("Combined", "Binary", "Nominal"))
# plot<-plot+scale_linetype_discrete(name="NA cost", breaks=c("0", "1"),labels=c("NA cost 0", "NA cost 2"))
# plot<-plot+coord_cartesian(xlim =c(1, 20),ylim=c(0.1,0.9)) 
# plot<-plot + scale_y_continuous(breaks=seq(0.1,0.8,0.1))
# plot<-plot+xlab("Number of Clusters")+ylab("PBC")
# plot<-plot + theme(axis.title.x = element_text(face='bold', size=13))
# plot<-plot + theme(axis.title.y = element_text(face='bold', size=13))
# PBCplot<-plot + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
# print(PBCplot)
# 
# #T-sne
#   #NA 0
#     #combined
# Rtsne0c<-Rtsne(Multiplot0, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne0c$Y, col=clusters0, main="Combined NA 0")
#     #binary
# Rtsne0b<-Rtsne(Multiplot0binary, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne0b$Y, col=clusters0binary, main="Binary NA 0")
#     #nominal
# Rtsne0n<-Rtsne(Multiplot0nominal, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne0n$Y, col=clusters0nominal, main="Nominal NA 0")
#   #NA 2
#     #combined
# Rtsne2c<-Rtsne(Multiplot2, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne2c$Y, col=clusters0, main="Combined NA 2")
#     #binary
# Rtsne2b<-Rtsne(Multiplot2binary, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne2b$Y, col=clusters2binary, main="Binary NA 2")
#     #nominal
# Rtsne2n<-Rtsne(Multiplot2nominal, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne2n$Y, col=clusters2nominal, main="Nominal NA 2")


# #Save to PDF
# pdf("seq.pdf",width=4,height=3,paper='special')
# newDF6<-map(newDF5, ~seqIplot(., sortv="from.start", with.legend=F))
# newDF7<-map(newDF5, ~seqdplot(., with.legend=F))
# dev.off()
# pdf("NA2clusters.pdf",width=4,height=4,paper='special')
# par(cex.main=.5, cex.lab=.5, cex.axis=.5, xaxs="r", yaxs="r", lwd=.5, mgp=c(0,0,0))
# plot(clusterward2)
# map2(newDF5, columnNames, ~seqdplot(.x, group = clusters2fac, border = NA, main=list(paste("NA2", .y), cex=.5)))
# map2(newDF5, columnNames,~seqfplot(.x, group = clusters2fac, border = NA, main=paste("NA2", .y)))
# map2(newDF5, columnNames,~seqIplot(.x, sortv="from.start", group = clusters2fac, border = NA, main=paste("NA2", .y)))
# dev.off()
# pdf("NA0clusters.pdf",width=4,height=4,paper='special')
# par(cex.main=.5, cex.lab=.5, xaxs=.5, yaxs=.5)
# plot(clusterward0)
# map2(newDF5, columnNames, ~seqdplot(.x, group = clusters0fac, border = NA, main=paste("NA0", .y),with.legend=F))
# map2(newDF5, columnNames,~seqfplot(.x, group = clusters0fac, border = NA, main=paste("NA0", .y),with.legend=F))
# map2(newDF5, columnNames,~seqIplot(.x, sortv="from.start", group = clusters0fac, border = NA, main=paste("NA0", .y),with.legend=F))
# dev.off()
# pdf("PBC-ASW.pdf")
# print(ASWplot)
# print(PBCplot)
# dev.off()
# pdf("T-sne 30 perplexity.pdf")
# Rtsne0c<-Rtsne(Multiplot0, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne0c$Y, col=clusters0, main="Combined NA 0")
# #binary
# Rtsne0b<-Rtsne(Multiplot0binary, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne0b$Y, col=clusters0binary, main="Binary NA 0")
# #nominal
# Rtsne0n<-Rtsne(Multiplot0nominal, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne0n$Y, col=clusters0nominal, main="Nominal NA 0")
# #NA 2
# #combined
# Rtsne2c<-Rtsne(Multiplot2, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne2c$Y, col=clusters0, main="Combined NA 2")
# #binary
# Rtsne2b<-Rtsne(Multiplot2binary, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne2b$Y, col=clusters2binary, main="Binary NA 2")
# #nominal
# Rtsne2n<-Rtsne(Multiplot2nominal, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
# plot(Rtsne2n$Y, col=clusters2nominal, main="Nominal NA 2")
# dev.off()





#Faster way to do above (pipeline)
# newDF<-columnNames %>%
#   map(., ~select(surveyData,id,age,.)) %>%
#   map2(.,columnNames,~spread(.x,age,.y)) %>%
#   map(.,~select(.,2:32)) %>%
#   map(.,~na_if(.,'.n')) %>%
#   map(.,~na_if(.,'.p')) %>%
#   map(.,~na_if(.,'.z')) %>%
#   map(.,~seqdef(.,xtstep = 5,start=agel,right=NA))

#NA 0
# subcost_NA0<- seqsubm(newDF5[[5]], method = "TRATE", with.missing=T, miss.cost=0)
# optimalMatchingResult0<- seqdist(newDF5[[5]], method = "OM", indel = 1, sm = subcost_NA0, with.missing=T)
# clusterward0 <- agnes(optimalMatchingResult0, diss = TRUE, method = "ward")
# plot(clusterward0)
# clusters0<-cutree(clusterward0, k=4)
# clusters0fac <- factor(clusters0, labels = paste("Type", 1:4))
# seqdplot(newDF5[[5]], group = clusters0fac, border = NA, main=paste("NA0", columnNames[[5]]))
# seqfplot(newDF5[[5]], group = clusters0fac, border = NA, main=paste("NA0", columnNames[[5]]))
# seqIplot(newDF5[[5]], sortv="from.start", group = clusters0fac, border = NA, main=paste("NA0", columnNames[[5]]))

# NA2
# subcost_NA2<- seqsubm(newDF5[[5]], method = "TRATE", with.missing=T, miss.cost=2)
# optimalMatchingResult2<- seqdist(newDF5[[5]], method = "OM", indel = 1, sm = subcost_NA2, with.missing=T)
# clusterward2 <- agnes(optimalMatchingResult2, diss = TRUE, method = "ward")
# plot(clusterward2)
# clusters2<-cutree(clusterward2, k=4)
# clusters2fac <- factor(clusters2, labels = paste("Type", 1:4))
# seqdplot(newDF5[[5]], group = clusters2fac, border = NA, main=paste("NA2", columnNames[[5]]))
# seqfplot(newDF5[[5]], group = clusters2fac, border = NA, main=paste("NA2", columnNames[[5]]))
# seqIplot(newDF5[[5]], sortv="from.start", group = clusters2fac, border = NA, main=paste("NA2", columnNames[[5]]))
