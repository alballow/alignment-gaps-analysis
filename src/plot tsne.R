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
load("temps/t-sne")
load("temps/newvariables")


DF<-select(reduced35, id, birthyear)
DF2<-distinct(DF)
DF2$agecohort<-factor(ifelse(DF2$birthyear>=1968, 0, 1))
table(DF2$agecohort)

table<-table(new_data$id)
table(table)

byn<-unique(select(reduced35,id,birthyear))
byn[(byn <= 1968)] = 0
byn[(byn > 1968 & byn<=1976)] = 1
byn[(byn>1976)]=2
byn<-factor(as.vector(byn$birthyear))

axistitle<- textGrob("Demographics                                  Travel", gp=gpar(fontface="bold"), rot=90)
perplexity<-c(30, 50, 70, 90, 100)


plottsneperp<-function(Rtsne, clusterward, NAcost, numCluster, title)
{
  xrange<-max(Rtsne$Y[,1])-min(Rtsne$Y[,1])
  yrange<-max(Rtsne$Y[,2])-min(Rtsne$Y[,2])
  clusters<-cutree(clusterward, k=numCluster)
  clustersfac<- factor(clusters, labels = paste("Cluster", 1:numCluster))
  tsne_plot <- data.frame(x = Rtsne$Y[,1], y = Rtsne$Y[,2], col = clustersfac)
  plot<-ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col))+scale_color_discrete(name="Clusters", breaks=c( 1:numCluster),labels=paste("Cluster", 1:numCluster))
  plot<-plot+theme(legend.position="none", plot.title = element_text(face="bold"), axis.title=element_blank(),axis.text.x = element_text(face="bold"),axis.text.y = element_text(face="bold"))+ggtitle(title)
  plot<-plot+ coord_fixed(ratio=xrange/yrange)
  return(plot)
}

# pdf("plots/tsne5clustersNA.1-.2perp30-100", height=8, width=36)
set10<-map2(set1tsne.1, perplexity,  ~plottsneperp(.x, clusterwardset1.4[[4]], 2, 4, .y))
set20<-map2(set2tsne.1, perplexity,  ~plottsneperp(.x, clusterwardset2.4[[4]], 2, 4, .y))
grid.arrange(set20[[1]], set20[[2]], set20[[3]], set20[[4]], set20[[5]],
             set10[[1]], set10[[2]], set10[[3]], set10[[4]], set10[[5]],
             top="NA 0.2",
             left=axistitle,
             ncol=5)  

set10<-map2(set1tsne.2, perplexity,  ~plottsneperp(.x, clusterwardset1.4[[3]], 2, 5, .y))
set20<-map2(set2tsne.2, perplexity,  ~plottsneperp(.x, clusterwardset2.4[[3]], 2, 5, .y))
grid.arrange(set20[[1]], set20[[2]], set20[[3]], set20[[4]], set20[[5]],
             set10[[1]], set10[[2]], set10[[3]], set10[[4]], set10[[5]],
             top="NA 0.1",
             left=axistitle,
             ncol=5)  
# dev.off()


plottsnena<-function(Rtsne,  NAcost, col, title)
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

NAcost<-c("0", "0.4", "0.8", "1.2", "1.6", "2")
ageincluded<-c(35, 37, 39, 41, 43, 45,47,49,50)
NAcost.4<-c(0, 0.1, 0.2, 0.3, 0.4)

tsne_plot <- data.frame(x = set1tsne[[1]]$Y[,1], y = set1tsne[[1]]$Y[,2], col = DF2$agecohort)
p1<-ggplot(data=tsne_plot, aes(x=x,y=y,group=DF2$agecohort))+ geom_point(aes(x=x, y=y, col=DF2$agecohort), alpha=I(0.3))+theme_minimal()+scale_color_discrete(name="Birth year", breaks=c("0", "1"), labels=c("<=1968", ">1968"))
legend2<-get_legend(p1 + theme(legend.position="bottom"))
tsne_plot <- data.frame(x = set1tsne[[1]]$Y[,1], y = set1tsne[[1]]$Y[,2], col = byn)
p1<-ggplot(data=tsne_plot, aes(x=x,y=y,group=byn))+ geom_point(aes(x=x, y=y, col=byn), alpha=I(0.3))+theme_minimal()+scale_color_discrete(name="Birth year", breaks=c("0", "1", "2"),labels=c("<=1968", "1968 ~ 1976", ">1976"))
legend3<-get_legend(p1 + theme(legend.position="bottom"))

# pdf("plots/tsneNA0-2age50.pdf", height=9, width=36)
set1varyna<-map2(set1tsne, NAcost, ~plottsnena(.x, NAcost, DF2$agecohort, paste("NA cost:", .y) ))
set2varyna<-map2(set2tsne, NAcost, ~plottsnena(.x, NAcost, DF2$agecohort, paste("NA cost:", .y)))
plots<-grid.arrange(set2varyna[[6]], set2varyna[[5]], set2varyna[[4]], set2varyna[[3]], set2varyna[[2]], set2varyna[[1]],
             set1varyna[[6]], set1varyna[[5]], set1varyna[[4]], set1varyna[[3]], set1varyna[[2]], set1varyna[[1]],
             left=axistitle,
             ncol=6)
plot_grid( plots, legend2, ncol = 1, rel_heights = c(1, .2))

set1varyna<-map2(set1tsne, NAcost, ~plottsnena(.x, NAcost, byn, paste("NA cost:", .y)))
set2varyna<-map2(set2tsne, NAcost, ~plottsnena(.x, NAcost, byn, paste("NA cost:", .y)))
plots<-grid.arrange(set2varyna[[6]], set2varyna[[5]], set2varyna[[4]], set2varyna[[3]], set2varyna[[2]], set2varyna[[1]],
             set1varyna[[6]], set1varyna[[5]], set1varyna[[4]], set1varyna[[3]], set1varyna[[2]], set1varyna[[1]],
             left=axistitle,
             ncol=6)
plot_grid( plots, legend3, ncol = 1, rel_heights = c(1, .2))
# dev.off()
# pdf("plots/tsneNA0-0.4age50.pdf", height=8, width=36)

set1varyna<-map2(set1tsne.4, NAcost.4, ~plottsnena(.x, NAcost, DF2$agecohort, paste("NA cost:", .y)))
set2varyna<-map2(set2tsne.4, NAcost.4, ~plottsnena(.x, NAcost, DF2$agecohort, paste("NA cost:", .y)))
plots<-grid.arrange(set2varyna[[5]], set2varyna[[4]], set2varyna[[3]], set2varyna[[2]], set2varyna[[1]],
             set1varyna[[5]], set1varyna[[4]], set1varyna[[3]], set1varyna[[2]], set1varyna[[1]],
             left=axistitle,
             ncol=5)
plot_grid( plots, legend2, ncol = 1, rel_heights = c(1, .2))

set1varyna<-map2(set1tsne.4, NAcost.4, ~plottsnena(.x, NAcost, byn, paste("NA cost:", .y)))
set2varyna<-map2(set2tsne.4, NAcost.4, ~plottsnena(.x, NAcost, byn, paste("NA cost:", .y)))
plots<-grid.arrange(set2varyna[[5]], set2varyna[[4]], set2varyna[[3]], set2varyna[[2]], set2varyna[[1]],
             set1varyna[[5]], set1varyna[[4]], set1varyna[[3]], set1varyna[[2]], set1varyna[[1]],
             left=axistitle,
             ncol=5)
plot_grid( plots, legend3, ncol = 1, rel_heights = c(1, .2))
# dev.off()
# pdf("plots/tsneage35-50NA2.pdf", height=9, width=36)

set1short<-map2(set1tsneshort, ageincluded, ~plottsnena(.x, clusterwardset1short, DF2$agecohort, .y))
set2short<-map2(set2tsneshort, ageincluded, ~plottsnena(.x, clusterwardset2short, DF2$agecohort, .y))
plots<-grid.arrange(set2short[[9]], set2short[[8]], set2short[[7]], set2short[[6]], set2short[[5]], set2short[[4]], set2short[[3]], set2short[[2]], set2short[[1]],
             set1short[[9]], set1short[[8]], set1short[[7]], set1short[[6]], set1short[[5]], set1short[[4]], set1short[[3]], set1short[[2]], set1short[[1]],
             left=axistitle,
             top= "T-sne plots with later years removed", ncol=9)
plot_grid( plots, legend2, ncol = 1, rel_heights = c(1, .2))

set1short<-map2(set1tsneshort, ageincluded, ~plottsnena(.x, clusterwardset1short, byn, .y))
set2short<-map2(set2tsneshort, ageincluded, ~plottsnena(.x, clusterwardset1short, byn, .y))
plots<-grid.arrange(set2short[[9]], set2short[[8]], set2short[[7]], set2short[[6]], set2short[[5]], set2short[[4]], set2short[[3]], set2short[[2]], set2short[[1]],
             set1short[[9]], set1short[[8]], set1short[[7]], set1short[[6]], set1short[[5]], set1short[[4]], set1short[[3]], set1short[[2]], set1short[[1]],
             left=axistitle,
             top= "T-sne plots with later years removed", ncol=9)
plot_grid( plots, legend3, ncol = 1, rel_heights = c(1, .2))
dev.off()




