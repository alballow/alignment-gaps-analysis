library(TraMineR)
library(tidyverse)
library(colorspace, RColorBrewer)
library(gridExtra)

setwd("~/Documents/my project/src/alignment-gaps-analysis")
load(file="temps/newvariables")



years<-c(1983, 1985, 1987, 1989, 1991)
columns<-c(16, 18, 20, 22, 24, 26, 28, 30, 32)
numfactors(2,2,2,2,5)


defineseq<-function(data, ncols, numfactors, choices)
{
newDF<-map(choices, ~select(data,id,age,.))
newDF2<-map2(newDF,choices,~spread(.x,age,.y))
newDF3<-map(newDF2,~select(.,2:ncols))
newDF5<-map(newDF3,~seqdef(.,xtstep = 5, start=20 ,right=NA, cpal=sequential_hcl(numfactors)))
return(newDF5)
}

set1<-defineseq(reduced35, 32, 2, c("ifchildunder19","employ", "school","partner"))
set2<-defineseq(reduced35, 32, 2, c("used_walkbike", "used_public", "used_own"))
# set1<-map(reduced, ~defineseq(., 32, 2, c("ifchildunder19","employ", "school","partner")))
# set2<-map(reduced, ~defineseq(., 32, 2, c("used_walkbike", "used_public", "used_own")))
shortenedset1<-map(columns, ~defineseq(reduced35, ., 2, c("ifchildunder19","employ", "school","partner")))
shortenedset2<-map(columns, ~defineseq(reduced35, ., 2, c("used_walkbike", "used_public", "used_own")))
total<-defineseq(reduced35, 32, 2, "employ")

seqIplot(total[[1]], sortv="from.end")

save(set1, set2, shortenedset1, shortenedset2, reduced35, file="temps/sets")


