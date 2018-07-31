library(TraMineR)
library(tidyverse)
library(colorspace, RColorBrewer)
library(Rtsne)
library(ggplot2)
library(gridExtra)
library(grid)

fileName<-'clean_reduced_lifehistory'
surveyData3<- read.csv("./data/clean_reduced_lifehistory.csv",check.names=F,header=TRUE,sep=",")
pathInput = paste0("./data/")
#filesData
fileWithPath<-paste0(pathInput,fileName,".csv")
filesData<-read.csv(fileWithPath,check.names=F,header=TRUE,sep=",")

filesData <- filesData %>% filter(birthyear<1984) 
reduced<-filesData

DF<-select(new_data, id, birthyear)
DF2<-distinct(DF)

DF2$agecohort<-ifelse(DF2$birthyear<1968, 0, 1)



# byn<-unique(select(reduced,id,birthyear))
# byn[(byn <= 1968)] = 1
# byn[(byn > 1968)] = 2
# byn<-as.vector(byn$birthyear)

setname<-c("youngchild","employ", "school","partner")

newDF<-map(setname, ~select(new_data,id,age,.))
newDF2<-map2(newDF, setname, ~spread(.x,age,.y))
newDF3<-map(newDF2, ~select(.,2:32))
newDF5<-map(newDF3, ~seqdef(., xtstep = 5, start=20,right=NA ))

subcost<-map(newDF5, ~seqsubm(., method="TRATE", with.missing=T, miss.cost=0))
multiplot<-seqdistmc(channels=newDF5, "OM", sm=subcost, with.missing=T, miss.cost=0)
multiplot<-as.dist(multiplot)

tsne<-Rtsne(multiplot, perplexity=30, dims = 2, initial_dims = 30,
            max_iter = 1000,  check_duplicates = FALSE,  is_distance = TRUE)


tsne_plot <- data.frame(x = tsne$Y[,1], y = tsne$Y[,2], col = DF2$agecohort)
ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col))+scale_colour_gradient(low = "Green", high = "Red")


setname<-c("youngchild","employ", "school","partner")

newDF<-map(setname, ~select(reduced,id,age,.))
newDF2<-map2(newDF, setname, ~spread(.x,age,.y))
newDF3<-map(newDF2, ~select(.,2:32))
newDF5<-map(newDF3, ~seqdef(., xtstep = 5, start=20 ,right=NA))

#subcost<-map(newDF5, ~seqsubm(., method="TRATE", with.missing=T, miss.cost=2))
#multiplot<-seqdistmc(channels=newDF5, "OM", sm=subcost, with.missing=T)
l4<-list("TRATE","TRATE","TRATE","TRATE")
nacost=2

multiplot<-seqdistmc(channels=newDF5, sm=l4, miss.cost=nacost,
                     method="OM",with.missing = T,norm="none")
multiplot<-as.dist(multiplot)

#multiplot2<-mcdist.om1[[2]]
#cluster<-agnes(multiplot, method="ward")
tsne<-Rtsne(multiplot, dims = 2, perplexity = 30,
            max_iter = 1000, check_duplicates=FALSE,  is_distance = TRUE)


tsne_plot <- data.frame(tsne$Y)
#plot(tsne_plot,col=by)
ggplot(tsne_plot) + geom_point(aes(x=tsne_plot[,1], y=tsne_plot[,2], color=byn))+
  scale_color_gradient(low = "Green", high = "Red")+
  theme(legend.position="none")








