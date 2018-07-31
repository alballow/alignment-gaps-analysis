library(tidyverse)

setwd("~/Documents/my project/src/alignment-gaps-analysis")

surveyData2<- read.csv("./data/clean_full_lifehistory.csv",check.names=F,header=TRUE,sep=",")
surveyData3<- read.csv("./data/clean_reduced_lifehistory (1).csv",check.names=F,header=TRUE,sep=",")

birthyear<-filter(surveyData3, child==1)
birthyear2<-select(birthyear, id, age, year)
birthyear3<-spread(birthyear2, age, year)
birthyear4<-select(birthyear3,2:32)
years<-apply(birthyear4, MARGIN = 1, na.omit)
birth_7<-map(years, ~lapply(., "+", 7))
birthyears5<-map(years, ~table(.))
id<-select(birthyear3, "id")

first<-list(c(1990:1997, 2005:2012, 2008:2015))
table(first)
table<- table(surveyData3$id)
table(table)
table<- table(surveyData2$id)
table(table)
table<- table(reduced35$id)
table(table)

age=1
age2=7
list(c(age:age+7))
list(c(age:age2))


expandarray1 <- function(year, amount)
{
  year2<-year+amount
  result<-(year:year2)
  return(result)
}

expandarray2<- function(list, amount)
{
    return(lists<-sapply(list, expandarray1, amount))
}
childunder8<-map(years, ~expandarray2(., amount=7))
nchildunder8<-map(childunder8, ~table(.))
childunder19<-map(years, ~expandarray2(., amount=18))
nchildunder19<-map(childunder19, ~table(.))

dataorder<- function(input, ids){
  results<-c()
    for (i in 1:404)
    {
      cu8<-as.data.frame(input[[i]])
      amount<-nrow(cu8)
      cu7<-rep(id[i, ], amount)
      cu7<-as.data.frame(cu7)
      result<-bind_cols(cu7, cu8)
      results<-bind_rows(results, result)
    }
  return(results)
}

as.integer(surveyData3$year)
data8<-dataorder(nchildunder8, id)
data19<-dataorder(nchildunder19, id)
colnames(data8)<-c("id","year","numchild8")
colnames(data19)<-c("id","year","numchild19")
new_data<-merge(surveyData3, data8, all.x=T)
new_data<-merge(new_data, data19, all.x=T)
new_data$numchild8[is.na(new_data$numchild8)]<-0
new_data$numchild19[is.na(new_data$numchild19)]<-0
new_data$numchild8[is.na(new_data$child)]<-NA
new_data$numchild19[is.na(new_data$child)]<-NA
new_data$ifchildunder8<-ifelse(new_data$numchild8>0, 1, 0)
new_data$ifchildunder19<-ifelse(new_data$numchild19>0, 1, 0)

new_data$newhhsize<-new_data$partner+new_data$numchild19+1
new_data$morethan2car<-ifelse(new_data$numcars>2, 1, 0)
new_data$morethan2hhsize<-ifelse(new_data$newhhsize>2, 1, 0)
  
comparison<-ifelse(reduced35$numchild8==reduced35$youngchild, T, F)
table(comparison)
ycvunder8<-filter(reduced35,{reduced35$ifchildunder8!=reduced35$youngchild} )
ycvunder8id<-select(ycvunder8, id)
ids<-distinct(ycvunder8id)
filter(reduced35, reduced35$id)
id<-as.vector(reduced35$id)
malfunction<-filter(reduced35, id %in% ids)

comparison01<-ifelse({new_data$numchild8==0 & new_data$youngchild==1}, T, F)
table(comparison01)
comparison10<-ifelse({new_data$numchild8==1 & new_data$youngchild==0}, T, F)
table(comparison10)

comparison01<-ifelse({new_data$numchild8==0 & new_data$youngchild==0}, T, F)
table(comparison01)
comparison10<-ifelse({new_data$numchild8==1 & new_data$youngchild==1}, T, F)
table(comparison10)
  
ycvunder8<-which({new_data$ifchildunder8==0 & new_data$youngchild==1})
table(ycvunder8, arr.ind=F)
under8vyc<-which({new_data$ifchildunder8==1 & new_data$youngchild==0})
table(under8vyc)

ycvunder8<-filter(new_data,{new_data$ifchildunder8==0 & new_data$youngchild==1} )
ycvunder8id<-select(ycvunder8, id)
table(ycvunder8id)

under8vyc<-filter(new_data,{new_data$ifchildunder8==1 & new_data$youngchild==0} )
under8vycid<-select(under8vyc, id)
table(under8vycid)

reduced<-merge(surveyData3, new_data, by="id")
ids<-select(new_data, id)
nids<-table(ids)
reduced35<-filter(new_data, new_data$birthyear<=1983)
ids<-select(reduced35, id)
nids<-table(ids)
maxreduced35$test<-ifelse({abs(maxreduced35$newhhsize-maxreduced35$hhsize)}>1, T, F)
maxreduced35short<-filter(maxreduced35, maxreduced35$test==T)
ids<-select(maxreduced35short, id)
seqIplot(reduced35)

comparisons<-select(new_data, id, youngchild, ifchildunder8)
reduced35<-filter(reduced35, !(id %in% ids$id))

save(new_data, reduced35, file="temps/newvariables")
write.csv(new_data, 'fulldata.csv')
write.csv(reduced35, "data/reduced35.csv")
write.csv(reduced, "reduced.csv")
write.csv(ids, "removedids.csv")



