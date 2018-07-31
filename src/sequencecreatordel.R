library(TraMineR)
library(tidyverse)
library(colorspace, RColorBrewer)
library(gridExtra)

setwd("~/Documents/my project/src/alignment-gaps-analysis")
load(file="temps/newvariables")

columns<-c(16, 18, 20, 22, 24, 26, 28, 30, 32)
column<-c(20, 21, 22)

defineseqdel<-function(data, ncols, numfactors, choices, norm)
{
  newDF<-map(choices, ~select(data,id,age,.))
  newDF2<-map2(newDF,choices,~spread(.x,age,.y))
  newDF3<-map(newDF2,~select(.,2:ncols))
  newDF5<-map(newDF3,~seqdef(.,xtstep = 5, start=20 ,right="DEL", norm=norm, cpal=sequential_hcl(numfactors)))
  return(newDF5)
}

set1del<-defineseqdel(reduced35, 32, 2, c("ifchildunder8","employ", "school","partner", NULL))
set2del<-defineseqdel(reduced35, 32, 2, c("used_walkbike", "used_public", "used_own"), NULL)
set1delnorm<-defineseqdel(reduced35, 32, 2, c("ifchildunder8","employ", "school","partner"), auto)
set2delnorm<-defineseqdel(reduced35, 32, 2, c("used_walkbike", "used_public", "used_own"), auto)
# set1<-map(reduced, ~defineseq(., 32, 2, c("ifchildunder19","employ", "school","partner")))
# set2<-map(reduced, ~defineseq(., 32, 2, c("used_walkbike", "used_public", "used_own")))
shortenedset1del<-map(columns, ~defineseqdel(reduced35, ., 2, c("ifchildunder8","employ", "school","partner"), NULL))
shortenedset2del<-map(columns, ~defineseqdel(reduced35, ., 2, c("used_walkbike", "used_public", "used_own"), NULL))
shortenedset1delnorm<-map(columns, ~defineseqdel(reduced35, ., 2, c("ifchildunder8","employ", "school","partner"), auto))
shortenedset2delnorm<-map(columns, ~defineseqdel(reduced35, ., 2, c("used_walkbike", "used_public", "used_own"), auto))
shortenedset1del40<-map(column, ~defineseqdel(reduced35, ., 2, c("ifchildunder8","employ", "school","partner"), NULL))
shortenedset2del40<-map(column, ~defineseqdel(reduced35, ., 2, c("used_walkbike", "used_public", "used_own"), NULL))

save(set1del, set2del, shortenedset1del, shortenedset2del, set1delnorm, set2delnorm, shortenedset1delnorm, shortenedset2delnorm, file="temps/setsdel")
save(shortenedset1del, shortenedset2del, file="temps/clustervaliditysets")
