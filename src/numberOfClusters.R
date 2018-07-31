library(TraMineR)
library(tidyverse)
library(colorspace, RColorBrewer)
library(cluster, factoextra)
library(WeightedCluster)

setwd("~/Documents/my project/src/alignment-gaps-analysis")
load(file="temps/Clusterinfo")
load(file="temps/newDF5")

#NA 0
  #combined
  clusterRange0<-as.clustrange(clusterward0, Multiplot0, nclusters=20)
  plot(clusterRange0, c("ASW", "PBC"))
  ASW0<-data.frame(clusterRange0$stats$ASW)
  PBC0<-data.frame(clusterRange0$stats$PBC)
  numberClusters<-data.frame(c(2:20))
  #Nominal
  clusterRange0nominal<-as.clustrange(clusterward0nominal, Multiplot0nominal, nclusters=20)
  ASW0nominal<-data.frame(clusterRange0nominal$stats$ASW)
  PBC0nominal<-data.frame(clusterRange0nominal$stats$PBC)
  #Binary
  clusterRange0binary<-as.clustrange(clusterward0binary, Multiplot0binary, nclusters=20)
  ASW0binary<-data.frame(clusterRange0binary$stats$ASW)
  PBC0binary<-data.frame(clusterRange0binary$stats$PBC)
#Na 2
  #combined
  clusterRange2<-as.clustrange(clusterward2, Multiplot2, nclusters=20)
  plot(clusterRange2, c("ASW", "PBC"))
  ASW2<-data.frame(clusterRange2$stats$ASW)
  PBC2<-data.frame(clusterRange2$stats$PBC)
  #Nominal
  clusterRange2nominal<-as.clustrange(clusterward2nominal, Multiplot2nominal, nclusters=20)
  ASW2nominal<-data.frame(clusterRange2nominal$stats$ASW)
  PBC2nominal<-data.frame(clusterRange2nominal$stats$PBC)
  #Binary
  clusterRange2binary<-as.clustrange(clusterward2binary, Multiplot2binary, nclusters=20)
  ASW2binary<-data.frame(clusterRange2binary$stats$ASW)
  PBC2binary<-data.frame(clusterRange2binary$stats$PBC)
#Data Arrangemnet
  #Combined
  ASWcombined<-bind_cols( ASW0, ASW2)
  PBCcombined<-bind_cols(PBC0,PBC2)
  #Binary
  ASWbinary<-bind_cols(ASW0binary, ASW2binary)
  PBCbinary<-bind_cols(PBC0binary,PBC2binary)
  #Nominal
  ASWnominal<-bind_cols(ASW0nominal, ASW2nominal)
  PBCnominal<-bind_cols(PBC0nominal,PBC2nominal)
  
  ASW<-bind_cols(numberClusters, ASWnominal,  ASWbinary, ASWcombined)
  PBC<-bind_cols(numberClusters, PBCnominal, PBCbinary, PBCcombined)
  
save(ASW, PBC, file="temps/plotinfo")  