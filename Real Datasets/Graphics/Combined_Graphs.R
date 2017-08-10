setwd("~/Box Sync/Iowa State/Research/CF Aggregation Paper/Code for Calibration Paper/Current Code/Combined_Graphics")
load("STEMResults.Rdata")
df$Method=as.character(df$Method)
df$Method[df$Method=="CF-P"]="Combined"
df$Method[df$Method=="CF-WP"]="cForest"
df$Method[df$Method=="RF-P"]="randomForest"
STEMdf=df
STEMPreds=TestPreds
load("CCDResults.Rdata")
df$Method=as.character(df$Method)
df$Method[df$Method=="CF-P"]="Combined"
df$Method[df$Method=="CF-WP"]="cForest"
df$Method[df$Method=="RF-P"]="randomForest"
CCDdf=df
CCDPreds=TestPreds

library(ggplot2)
########################################################
#Figure 4-Calibration plots
p1=qplot(data=CCDdf, x=Nodesize, y=NdMean, colour=Method, xlim=c(0,500), ylim=c(0.15,0.24), xlab="Nodesize", ylab="Mean Prob. Est.", main="Credit Card Default")+ geom_hline(aes(yintercept=mean(CCDPreds[,,1,,])))+theme_bw()+ theme(legend.position = "bottom")+theme(plot.title = element_text(hjust = 0.5))+geom_line()+theme(axis.text=element_text(size=12))+theme(axis.title.x=element_text(size=12))+theme(axis.title.y=element_text(size=12))+theme(legend.text=element_text(size=12))
p2=qplot(data=STEMdf, x=Nodesize, y=NdMean, colour=Method, xlim=c(0,1000), ylim=c(0.06,0.11), xlab="Nodesize", ylab="Mean Prob. Est.", main="STEM")+ geom_hline(aes(yintercept=mean(STEMPreds[,1,,,,])))+geom_line()+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=12))+theme(axis.title=element_text(size=12))+theme(axis.title.x=element_text(size=12))+theme(axis.title.y=element_text(size=12))+theme(legend.text=element_text(size=12))

library(gridExtra)
#Get legend at bottom of all plots
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)

p <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none")+theme(axis.text=element_text(size=12)),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 2))

tiff('Cal.tiff', units="in", width=7, height=4, res=600, compression = 'lzw')
grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         p2 + theme(legend.position="none")+theme(axis.text=element_text(size=12)),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 2))
dev.off()

########################################################
#Figure 5
#Log Loss
#Plot average estimated probability against nodesize for each technique
p1=qplot(data=CCDdf, x=Nodesize, y=NdSzError, colour=Method, xlim=c(0,500),ylim=c(1273,1550) ,xlab="Nodesize", ylab="Sum of Log Loss", main="Credit Card Default")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="none")+geom_line()
p2=qplot(data=CCDdf, x=Nodesize, y=NdSzError, colour=Method, xlim=c(0,500),ylim=c(1273,1325) ,xlab="Nodesize", ylab="Sum of Log Loss", main="Credit Card Default")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="none")+geom_line()
p3=qplot(data=STEMdf, x=Nodesize, y=NdSzError, colour=Method, xlim=c(0,1000) ,xlab="Nodesize", ylab="Sum of Log Loss", main="STEM")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+geom_line()+ theme(legend.position="none")
p4=qplot(data=STEMdf, x=Nodesize, y=NdSzError, colour=Method, xlim=c(0,1000),ylim=c(290,305) ,xlab="Nodesize", ylab="Sum of Log Loss", main="STEM")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+geom_line()+ theme(legend.position="none")

library(cowplot)
p5=plot_grid(p1,p3,p2,p4, labels=c("a)", "b)", "c)", "d)"), ncol = 2, nrow = 2)

#Use for grid
p1a=qplot(data=CCDdf, x=Nodesize, y=NdSzError, colour=Method, xlim=c(0,500),ylim=c(1273,1550) ,xlab="Nodesize", ylab="Sum of Log Loss", main="Credit Card Default")+theme(plot.title = element_text(hjust = 0.5))+geom_line()+ theme(legend.position="bottom")+ theme(legend.text=element_text(size=12), legend.title=element_text(size=12))

library(gridExtra)
#Get legend at bottom of all plots
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1a)

tiff('LL.tiff', units="in", width=7, height=4, res=600, compression = 'lzw')
p <- grid.arrange(arrangeGrob(p5 + theme(legend.position="none"),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 1))
dev.off()


###################################################################################
#Figure 6 Loss Using unbalanced penalty
load("PendfCCD")
Pendf$Method=as.character(Pendf$Method)
Pendf$Method[Pendf$Method=="CF-P"]="Combined"
Pendf$Method[Pendf$Method=="CF-WP"]="cForest"
Pendf$Method[Pendf$Method=="RF-P"]="randomForest"
PendfCCD=Pendf

load("PendfSTEM")
Pendf$Method=as.character(Pendf$Method)
Pendf$Method[Pendf$Method=="CF-TP"]="Combined"
Pendf$Method[Pendf$Method=="CF-WTP"]="cForest"
Pendf$Method[Pendf$Method=="RF-TP"]="randomForest"
PendfSTEM=Pendf

names(PendfCCD)=c("Alpha", "Ratio", "Method")
p1=qplot(data=PendfCCD, x=Alpha, y=(Ratio-1)*100, colour=Method, xlim=c(1,5), ylim=c(0,2), main="CCD", ylab="% Inc. in Loss", xlab=expression(alpha))+theme_bw()+geom_line()+ theme(legend.position = "bottom")+theme(plot.title = element_text(hjust = 0.5))+geom_line()+theme(axis.text=element_text(size=12))+theme(axis.title.x=element_text(size=12))+theme(axis.title.y=element_text(size=12))+theme(legend.text=element_text(size=12))
names(PendfSTEM)=c("Alpha", "Ratio", "Method")
p2=qplot(data=PendfSTEM, x=Alpha, y=(Ratio-1)*100, colour=Method, xlim=c(1,5), ylim=c(0,3), main="STEM", ylab="% Inc. in Loss", xlab=expression(alpha))+theme_bw()+geom_line()+ theme(legend.position = "bottom")+theme(plot.title = element_text(hjust = 0.5))+geom_line()+theme(axis.text=element_text(size=12))+theme(axis.title.x=element_text(size=12))+theme(axis.title.y=element_text(size=12))+theme(legend.text=element_text(size=12))

library(gridExtra)
#Get legend at bottom of all plots
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)

tiff('PenLoss.tiff', units="in", width=7, height=4, res=600, compression = 'lzw')
p <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 2))
dev.off()
