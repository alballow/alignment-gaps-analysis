library(TraMineR)
library(tidyverse)
library(colorspace, RColorBrewer)
library(Rtsne)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
library(gridGraphics)

setwd("~/Documents/my project/src/alignment-gaps-analysis")
load("temps/t-snedel")
load("temps/newvariables")

DF<-select(reduced35, id, birthyear)
DF2<-distinct(DF)
DF2$agecohort<-factor(ifelse(DF2$birthyear>=1968, 0, 1))

byn<-unique(select(reduced35,id,birthyear))
byn[(byn <= 1968)] = 0
byn[(byn > 1968 & byn<=1976)] = 1
byn[(byn>1976)]=2
byn<-factor(as.vector(byn$birthyear))

axistitle<- textGrob("Demographics                                  Travel", gp=gpar(fontface="bold"), rot=90)

tsne_plot <- data.frame(x = set1tsneshortdel[[1]]$Y[,1], y = set1tsneshortdel[[1]]$Y[,2], col = DF2$agecohort)
p1<-ggplot(data=tsne_plot, aes(x=x,y=y,group=DF2$agecohort))+ geom_point(aes(x=x, y=y, col=DF2$agecohort), alpha=I(0.3))+theme_minimal()+scale_color_discrete(name="Birth year", breaks=c("0", "1"), labels=c("<=1968", ">1968"))
legend2<-get_legend(p1 + theme(legend.position="bottom", plot.background=element_rect("#daedefff")))
tsne_plot <- data.frame(x = set1tsneshortdel[[1]]$Y[,1], y = set1tsneshortdel[[1]]$Y[,2], col = byn)
p1<-ggplot(data=tsne_plot, aes(x=x,y=y,group=byn))+ geom_point(aes(x=x, y=y, col=byn), alpha=I(0.3))+theme_minimal()+scale_color_discrete(name="Birth year", breaks=c("0", "1", "2"),labels=c("<=1968", "1968 ~ 1976", ">1976"))
legend3<-get_legend(p1 + theme(legend.position="bottom", plot.background=element_rect("#daedefff")))

numclust<-c(2:8)
plottsneperp<-function(Rtsne, clusterward, numCluster, title)
{
  xrange<-max(Rtsne$Y[,1])-min(Rtsne$Y[,1])
  yrange<-max(Rtsne$Y[,2])-min(Rtsne$Y[,2])
  clusters<-cutree(clusterward, k=numCluster)
  clustersfac<- factor(clusters, labels = paste("Cluster", 1:numCluster))
  tsne_plot <- data.frame(x = Rtsne$Y[,1], y = Rtsne$Y[,2], col = clustersfac)
  plot<-ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col))+scale_color_discrete(name="Clusters", breaks=c( 1:numCluster),labels=paste("Cluster", 1:numCluster))
  plot<-plot+theme(legend.position="none", plot.background=element_rect(fill="#daedefff", color="#daedefff"), plot.title = element_text(face="bold"), axis.title=element_blank(),axis.text.x = element_text(face="bold"),axis.text.y = element_text(face="bold"))+ggtitle(title)
  plot<-plot+ coord_fixed(ratio=xrange/yrange)
  return(plot())
}

clusterfacset13clusters<-plottsneperp(set1tsneshortdel[[3]], clusterwardset1shortdel[[3]], 3, 3)
clusterfacset23clusters<-plottsneperp(set2tsneshortdel[[3]], clusterwardset2shortdel[[3]], 3, 3)
clusterfacset14clusters<-plottsneperp(set1tsneshortdel[[3]], clusterwardset1shortdel[[3]], 4, 4)
clusterfacset24clusters<-plottsneperp(set2tsneshortdel[[3]], clusterwardset2shortdel[[3]], 4, 4)
save(clusterfacset13clusters, clusterfacset23clusters, clusterfacset14clusters, clusterfacset24clusters, file="temps/clustervalidityinformation")

# pdf("plots/tsne2-8clustersdel", height=8, width=36)
set1clust<-map(numclust, ~plottsneperp(set1tsneshortdel[[3]], clusterwardset1shortdel[[3]], ., .))
set2clust<-map(numclust, ~plottsneperp(set2tsneshortdel[[3]], clusterwardset2shortdel[[3]], ., .))
grid.arrange(set2clust[[1]], set2clust[[2]], set2clust[[3]], set2clust[[4]], set2clust[[5]], set2clust[[6]], set2clust[[7]],
             set1clust[[1]], set1clust[[2]], set1clust[[3]], set1clust[[4]], set1clust[[5]], set1clust[[6]], set1clust[[7]],
             top="DEL maxlength",
             left=axistitle,
             ncol=7) 
grid.arrange(set2clust[[1]], set2clust[[3]], set2clust[[4]], set2clust[[5]], set2clust[[7]],
             set1clust[[1]], set1clust[[3]], set1clust[[4]], set1clust[[5]], set1clust[[7]],
             top="DEL maxlength",
             left=axistitle,
             ncol=5) 
dev.off()
plottsnena<-function(Rtsne, col, title)
{
  #clusters<-cutree(clusterward, k=numCluster)
  xrange<-max(Rtsne$Y[,1])-min(Rtsne$Y[,1])
  yrange<-max(Rtsne$Y[,2])-min(Rtsne$Y[,2])
  tsne_plot <- data.frame(x = Rtsne$Y[,1], y = Rtsne$Y[,2], col = col)
  plot<-ggplot(data=tsne_plot, aes(x=x,y=y,group=col)) + geom_point(aes(x=x, y=y, color=col), alpha=I(0.3))+scale_color_discrete(name="Birth year", breaks=c("0", "1"),labels=c("<=1968", ">1968"))+ggtitle(title)
  plot<-plot+theme(legend.position="none",panel.grid.minor=element_blank(), plot.title = element_text(face="bold"), axis.title=element_blank(),axis.text.x = element_text(face="bold"),axis.text.y = element_text(face="bold"))
  plot<-plot+ coord_fixed(ratio=xrange/yrange) 
  return(plot)
}

ageincluded<-c(35, 37, 39, 41, 43, 45,47,49,50)

# pdf("plots/tsneage35-50delnone-maxlength.pdf", height=8, width=36)

set1shortdel<-map2(set1tsneshortdel, ageincluded, ~plottsnena(.x, DF2$agecohort, .y))
set2shortdel<-map2(set2tsneshortdel, ageincluded, ~plottsnena(.x, DF2$agecohort, .y))
plots<-grid.arrange(set2shortdel[[9]], set2shortdel[[8]], set2shortdel[[7]], set2shortdel[[6]], set2shortdel[[5]], set2shortdel[[4]], set2shortdel[[3]], set2shortdel[[2]], set2shortdel[[1]],
             set1shortdel[[9]], set1shortdel[[8]], set1shortdel[[7]], set1shortdel[[6]], set1shortdel[[5]], set1shortdel[[4]], set1shortdel[[3]], set1shortdel[[2]], set1shortdel[[1]],
             left=axistitle,
             top= "T-sne plots with later years removed", ncol=9)
plot_grid( plots, legend2, ncol = 1, rel_heights = c(1, .2))

set1shortdel<-map2(set1tsneshortdel, ageincluded, ~plottsnena(.x, byn, .y))
set2shortdel<-map2(set2tsneshortdel, ageincluded, ~plottsnena(.x, byn, .y))
plots2<-grid.arrange(set2shortdel[[9]], set2shortdel[[8]], set2shortdel[[7]], set2shortdel[[6]], set2shortdel[[5]], set2shortdel[[4]], set2shortdel[[3]], set2shortdel[[2]], set2shortdel[[1]],
             set1shortdel[[9]], set1shortdel[[8]], set1shortdel[[7]], set1shortdel[[6]], set1shortdel[[5]], set1shortdel[[4]], set1shortdel[[3]], set1shortdel[[2]], set1shortdel[[1]],
             left=axistitle,
             top= "T-sne plots with later years removed", ncol=9)
plot_grid( plots2, legend3, ncol = 1, rel_heights = c(1, .2))


set1shortdel<-map2(set1tsneshortdelnormlength, ageincluded, ~plottsnena(.x, DF2$agecohort, .y))
set2shortdel<-map2(set2tsneshortdelnormlength, ageincluded, ~plottsnena(.x, DF2$agecohort, .y))
plots3<-grid.arrange(set2shortdel[[9]], set2shortdel[[8]], set2shortdel[[7]], set2shortdel[[6]], set2shortdel[[5]], set2shortdel[[4]], set2shortdel[[3]], set2shortdel[[2]], set2shortdel[[1]],
                     set1shortdel[[9]], set1shortdel[[8]], set1shortdel[[7]], set1shortdel[[6]], set1shortdel[[5]], set1shortdel[[4]], set1shortdel[[3]], set1shortdel[[2]], set1shortdel[[1]],
                     left="   Set 1                                                             Set 2",
                     top= "T-sne plots with later years removed", ncol=9)
plot_grid( plots3, legend2, ncol = 1, rel_heights = c(1, .2))

set1shortdel<-map2(set1tsneshortdelnormlength, ageincluded, ~plottsnena(.x, byn, .y))
set2shortdel<-map2(set2tsneshortdelnormlength, ageincluded, ~plottsnena(.x, byn, .y))
plots4<-grid.arrange(set2shortdel[[9]], set2shortdel[[8]], set2shortdel[[7]], set2shortdel[[6]], set2shortdel[[5]], set2shortdel[[4]], set2shortdel[[3]], set2shortdel[[2]], set2shortdel[[1]],
                     set1shortdel[[9]], set1shortdel[[8]], set1shortdel[[7]], set1shortdel[[6]], set1shortdel[[5]], set1shortdel[[4]], set1shortdel[[3]], set1shortdel[[2]], set1shortdel[[1]],
                     left=axistitle,
                     top= "T-sne plots with later years removed", ncol=9)
plot_grid( plots4, legend3, ncol = 1, rel_heights = c(1, .2))

# dev.off()
# # pdf("plots/tsneNA0-0.4del.pdf", height=8, width=36)
# 
# set1varyna<-map2(set1tsne.4, NAcost.4, ~plottsnena(.x, DF2$agecohort, paste("NA cost:", .y)))
# set2varyna<-map2(set2tsne.4, NAcost.4, ~plottsnena(.x, DF2$agecohort, paste("NA cost:", .y)))
# plots<-grid.arrange(set2varyna[[5]], set2varyna[[4]], set2varyna[[3]], set2varyna[[2]], set2varyna[[1]],
#                     set1varyna[[5]], set1varyna[[4]], set1varyna[[3]], set1varyna[[2]], set1varyna[[1]],
#                     left=axistitle, 
#                     ncol=5)
# plot_grid( plots, legend2, ncol = 1, rel_heights = c(1, .2))
# 
# set1varyna<-map2(set1tsne.4, ~plottsnena(.x, NAcost, byn, paste("NA cost:", .y)))
# set2varyna<-map2(set2tsne.4, ~plottsnena(.x, NAcost, byn, paste("NA cost:", .y)))
# plots<-grid.arrange(set2varyna[[5]], set2varyna[[4]], set2varyna[[3]], set2varyna[[2]], set2varyna[[1]],
#                     set1varyna[[5]], set1varyna[[4]], set1varyna[[3]], set1varyna[[2]], set1varyna[[1]],
#                     left=axistitle, 
#                     ncol=5)
# plot_grid( plots, legend3, ncol = 1, rel_heights = c(1, .2))

# # dev.off()
# pdf("plots/Age39-41norm:none-maxlength")
# set1short<-map2(set1tsneshortdel40, c(39, 40, 41), ~plottsnena(.x, byn, .y))
# set2short<-map2(set2tsneshortdel40, c(39, 40, 41), ~plottsnena(.x, byn, .y))
# plots<-grid.arrange(set2short[[3]], set2short[[2]], set2short[[1]],
#                     set1short[[3]], set1short[[2]], set1short[[1]],
#                     left=axistitle,
#                     top= "None", ncol=3)
# plot_grid( plots, legend3, ncol = 1, rel_heights = c(1, .2))
# set1short<-map2(set1tsneshortdelnormlength40, c(39, 40, 41), ~plottsnena(.x, byn, .y))
# set2short<-map2(set2tsneshortdelnormlength40, c(39, 40, 41), ~plottsnena(.x, byn, .y))
# plots2<-grid.arrange(set2short[[3]], set2short[[2]], set2short[[1]],
#                     set1short[[3]], set1short[[2]], set1short[[1]],
#                     left=axistitle,
#                     top= "maxlength", ncol=3)
# plot_grid( plots2, legend3, ncol = 1, rel_heights = c(1, .2))
# # dev.off()

# pdf("plots/Normalizationtest.pdf", height = 8, width=36)

set1shortdel<-map2(set1tsneshortdel, ageincluded, ~plottsnena(.x, byn, .y))
set2shortdel<-map2(set2tsneshortdel, ageincluded, ~plottsnena(.x, byn, .y))
plots<-grid.arrange(set2shortdel[[9]], set2shortdel[[8]], set2shortdel[[7]], set2shortdel[[6]], set2shortdel[[5]], set2shortdel[[4]], set2shortdel[[3]], set2shortdel[[2]], set2shortdel[[1]],
                    set1shortdel[[9]], set1shortdel[[8]], set1shortdel[[7]], set1shortdel[[6]], set1shortdel[[5]], set1shortdel[[4]], set1shortdel[[3]], set1shortdel[[2]], set1shortdel[[1]],
                    left=axistitle, 
                    top= "None", ncol=9)
plot_grid( plots, legend3, ncol = 1, rel_heights = c(1, .2))

set1shortdel<-map2(set1tsneshortdelnormlength, ageincluded, ~plottsnena(.x, byn, .y))
set2shortdel<-map2(set2tsneshortdelnormlength, ageincluded, ~plottsnena(.x, byn, .y))
plots<-grid.arrange(set2shortdel[[9]], set2shortdel[[8]], set2shortdel[[7]], set2shortdel[[6]], set2shortdel[[5]], set2shortdel[[4]], set2shortdel[[3]], set2shortdel[[2]], set2shortdel[[1]],
                    set1shortdel[[9]], set1shortdel[[8]], set1shortdel[[7]], set1shortdel[[6]], set1shortdel[[5]], set1shortdel[[4]], set1shortdel[[3]], set1shortdel[[2]], set1shortdel[[1]],
                    left=axistitle, 
                    top= "maxlength", ncol=9)
plot_grid( plots, legend3, ncol = 1, rel_heights = c(1, .2))

# set1short<-map2(set1tsneshortdelnormdist, ageincluded, ~plottsnena(.x, clusterwardset1shortnormdist, byn, .y))
# set2short<-map2(set2tsneshortdelnormdist, ageincluded, ~plottsnena(.x, clusterwardset2shortnormdist, byn, .y))
# plots<-grid.arrange(set2short[[9]], set2short[[7]], set2short[[5]], set2short[[4]], set2short[[3]], set2short[[1]],
#              set1short[[9]], set1short[[7]], set1short[[5]], set1short[[4]], set1short[[3]], set1short[[1]],
#              left=axistitle, 
#              top= "maxdist", ncol=6)
# plot_grid( plots, legend3, ncol = 1, rel_heights = c(1, .2))
# 
# set1short<-map2(set1tsneshortdelnormmean, ageincluded, ~plottsnena(.x, clusterwardset1shortnormmean, byn, .y))
# set2short<-map2(set2tsneshortdelnormmean, ageincluded, ~plottsnena(.x, clusterwardset2shortnormmean, byn, .y))
# plots<-grid.arrange(set2short[[9]], set2short[[7]], set2short[[5]], set2short[[4]], set2short[[3]], set2short[[1]],
#              set1short[[9]], set1short[[7]], set1short[[5]], set1short[[4]], set1short[[3]], set1short[[1]],
#              left=axistitle, 
#              top= "gmean", ncol=6)
# plot_grid( plots, legend3, ncol = 1, rel_heights = c(1, .2))
# 
# set1short<-map2(set1tsneshortdelnormy, ageincluded, ~plottsnena(.x, clusterwardset1shortnormy, byn, .y))
# set2short<-map2(set2tsneshortdelnormy, ageincluded, ~plottsnena(.x, clusterwardset2shortnormy, byn, .y))
# plots<-grid.arrange(set2short[[9]], set2short[[7]], set2short[[5]], set2short[[4]], set2short[[3]], set2short[[1]],
#                     set1short[[9]], set1short[[7]], set1short[[5]], set1short[[4]], set1short[[3]], set1short[[1]],
#                     left=axistitle, 
#                     top= "YujianBo", ncol=6)
# plot_grid( plots, legend3, ncol = 1, rel_heights = c(1, .2))

# dev.off()






# pdf("plots/June27_3.1", height=8, width=24)
# grid.arrange(set2varyna[[6]], set2varyna[[5]], set2varyna[[4]], set2varyna[[3]], set2varyna[[2]], set2varyna[[1]],
#              set1varyna[[6]], set1varyna[[5]], set1varyna[[4]], set1varyna[[3]], set1varyna[[2]], set1varyna[[1]],
#              left="   Set 1                                                             Set 2", 
#              top= "T-sne plots\n       NA 2                     NA 1.6                  NA 1.2                     NA 0.8                  NA 0.4                    NA 0"
#              , ncol=6)
# dev.off()
# pdf("plots/June27_3.2del3", height = 8, width = 36)
# grid.arrange(set2shortdel[[9]], set2shortdel[[8]], set2shortdel[[7]], set2shortdel[[6]], set2shortdel[[5]], set2shortdel[[4]], set2shortdel[[3]], set2shortdel[[2]], set2shortdel[[1]],
#              set1shortdel[[9]], set1shortdel[[8]], set1shortdel[[7]], set1shortdel[[6]], set1shortdel[[5]], set2shortdel[[4]], set2shortdel[[3]], set2shortdel[[2]], set2shortdel[[1]],
#              left="   Set 1                                                             Set 2", 
#              top= "T-sne plots with later years removed\n             35                                        37                                     39                                        41                                    43                                           45                                       47                                   49                                   50", ncol=9)
# dev.off()  
# 
# png("plots/Varied by NA cost", width = 1340, height = 480, units = "px", pointsize = 12)
# grid.arrange(set2varyna[[1]], set2varyna[[2]], set2varyna[[3]], set2varyna[[4]], set2varyna[[5]], set2varyna[[6]],
#              set1varyna[[1]], set1varyna[[2]], set1varyna[[3]], set1varyna[[4]], set1varyna[[5]], set1varyna[[6]],
#              left="   Set 1                                                             Set 2", 
#              top= "T-sne plots\n       NA 0                  NA 0.4               NA 0.8                  NA 1.2               NA 1.6                 NA 2"
#              , ncol=6)
# dev.off()
