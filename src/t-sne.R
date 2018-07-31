library(TraMineR)
library(tidyverse)
library(colorspace, RColorBrewer)
library(Rtsne)
library(ggplot2)

setwd("~/Documents/my project/src/alignment-gaps-analysis")
surveyData <- read.csv("./data/phase1.csv",check.names=F,header=TRUE,sep=",")
load("temps/Clusterinfo")

#T-sne
  #NA 0
    #combined
    Rtsne0c<-Rtsne(Multiplot0, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
    plot(Rtsne0c$Y, col=clusters0, main="Combined NA 0")
    #binary
    Rtsne0b<-Rtsne(Multiplot0binary, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
    plot(Rtsne0b$Y, col=clusters0binary, main="Binary NA 0")
    #nominal
    Rtsne0n<-Rtsne(Multiplot0nominal, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
    plot(Rtsne0n$Y, col=clusters0nominal, main="Nominal NA 0")
  #NA 2
    #combined
    Rtsne2c<-Rtsne(Multiplot2, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
    plot(Rtsne2c$Y, col=clusters0, main="Combined NA 2")
    #binary
    Rtsne2b<-Rtsne(Multiplot2binary, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
    plot(Rtsne2b$Y, col=clusters2binary, main="Binary NA 2")
    #nominal
    Rtsne2n<-Rtsne(Multiplot2nominal, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
    plot(Rtsne2n$Y, col=clusters2nominal, main="Nominal NA 2")
    
    surveyData$agecohort<-ifelse(surveyData$birthyear<1968, 0, 1)
    plottsnena<-function(Rtsne, clusterward, NAcost, numCluster, perplexity)
    {
      clusters<-cutree(clusterward, k=numCluster)
      tsne_plot <- data.frame(x = Rtsne$Y[,1], y = Rtsne$Y[,2], col = surveyData$agecohort)
      ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col))+scale_colour_gradient(low = "Green", high = "Red")+theme(legend.position="none")
    }
    tsnec0<-plottsnena(Rtsne0c,  clusterward0, 0 , 4)
    tsnec2<-plottsnena(Rtsne2c, clusterward2, 2, 4)
    grid.arrange(tsnec2, tsnec0,
                 left="   Set 1                                                             Set 2", 
                 top= "T-sne plots",  ncol=2)

pdf("plots/T-sne 30 perplexity.pdf")
Rtsne0c<-Rtsne(Multiplot0, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
plot(Rtsne0c$Y, col=clusters0, main="Combined NA 0")
#binary
Rtsne0b<-Rtsne(Multiplot0binary, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
plot(Rtsne0b$Y, col=clusters0binary, main="Binary NA 0")
#nominal
Rtsne0n<-Rtsne(Multiplot0nominal, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
plot(Rtsne0n$Y, col=clusters0nominal, main="Nominal NA 0")
#NA 2
#combined
Rtsne2c<-Rtsne(Multiplot2, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
plot(Rtsne2c$Y, col=clusters0, main="Combined NA 2")
#binary
Rtsne2b<-Rtsne(Multiplot2binary, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
plot(Rtsne2b$Y, col=clusters2binary, main="Binary NA 2")
#nominal
Rtsne2n<-Rtsne(Multiplot2nominal, dims = 2, initial_dims = 30, perplexity = 30,max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)
plot(Rtsne2n$Y, col=clusters2nominal, main="Nominal NA 2")
dev.off()