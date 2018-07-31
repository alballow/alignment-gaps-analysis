library(tidyverse)
surveyData2<- read.csv("./data/clean_full_lifehistory.csv",check.names=F,header=TRUE,sep=",")
surveyData3<- read.csv("./data/clean_reduced_lifehistory.csv",check.names=F,header=TRUE,sep=",")
load(file="temps/newvariables")

comparison<-ifelse(reduced35$numchild8==reduced35$youngchild, T, F)
table(comparison)
ycvunder8<-filter(reduced35,{reduced35$ifchildunder8!=reduced35$youngchild} )
ycvunder8id<-select(ycvunder8, id)
ids<-distinct(ycvunder8id)
ids<-as.vector(ids)
ids<-as.list(ids)
filter(reduced35, reduced35$id)
id<-distinct(as.vector(reduced35$id))
problemsreduced35<-filter(reduced35, id %in% ids$id)

malfunction<-reduced35[id %in% ids, ]
ifelse({reduced35$youngchild==c(1,1,1,1,1,1,1,1,1) & reduced35$ifchildunder8==c(1,1,1,1,1,1,1,1,0)}, T, F)


maxreduced35<-aggregate(.~problemsreduced35$id,  problemsreduced35,FUN= max, na.action=NULL)
maxreduced35<-select(maxreduced35, id, youngchild, partner, hhsize, numchild19)
maxreduced35$test<-ifelse((maxreduced35$youngchild==0 & maxreduced35$hhsize>=1), 1, 0)
maxreduced35short<-filter(maxreduced35, maxreduced35$test==1)

test<-lapply(maxreduced35,function(x) if(maxreduced35$youngchild==0 & maxreduced35$hhsize<=1) maxreduced35$ifchildunder8 = 0)

