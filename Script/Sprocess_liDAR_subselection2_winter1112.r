# 13 November 2012, Simon Filhol,
# 
# Script to derive snow depth map from LiDAR winter 2011/2011 at Glenn Creek
# 
# MAKE SURE TO LOAD FUNCTIONs:
# Set path and load required functions ====

ifelse({Sys.info()['sysname']=="Windows"},{
  setwd("G:/GlennCreek/Grd_LiDAR/Glenn_Creek1112/UTM/subselection_2/Clean")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/FitPlane.r")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/Basic_statmap_ptcloud.r")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/LoadPointCloud.r")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/Multiplot.r")
  Data.path <- "G:/GlennCreek/Grd_LiDAR/Glenn_Creek1112/UTM/subselection_2/Clean"
  },{
setwd("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Winter 11_12/Lidar Survey/UTM all/subselection_2/Clean")
 source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/Basic_statmap_ptcloud.r")
 source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/LoadPointCloud.r")
 source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/Multiplot.R")
 source("/Users/simonfilhol/github/local/My_R_script/Function/FitPlane.R")
Data.path <- "/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Winter 11_12/Lidar Survey/UTM all/subselection_2/Clean"
})

Data.prepare <- function(my.data,xlim,ylim,dx,dy,return.xyz){
  xyz <- my.data$Data
  translation <- my.data$Translation
  rm(my.data)
  
  xyz <- xyz[xyz[,1]>=xlim[1] & xyz[,1] <=xlim[2],]
  xyz <- xyz[xyz[,2]>=ylim[1] & xyz[,2] <=ylim[2],]
  
  stat <- Basic.statmap.ptcloud(xyz,dx,dy,xlim,ylim)
  if(missing(return.xyz)){return.xyz=FALSE}
  if(return.xyz==FALSE){
    rm(xyz)
    All <- list(Stat= stat,
                Trans=translation)}
  else{
    All <- list(Stat= stat,
                Trans=translation,
                xyz=xyz)
  }
  return(All)
}

#
#Loading and preparing data ====
# Boundary of area of interest (xlim,ylim), and resolution fo final product (dx,dy)
xlim <-c(0,12.5)
ylim <- c(0,12.5)
dx <- 0.05   # in meter
dy <- 0.05   # in meter

# Load data for the 23 Sept (A.):
my.data <- LoadPointCloud(file=paste(Data.path,"/QT23sep_GCS.xyz",sep=""))
A <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=TRUE)
A.stat <- A$Stat
A.trans <- A$Trans

# Load data for the 23 Oct (B.):
my.data <- LoadPointCloud(Trans=A.trans,file=paste(Data.path,"/QT23oct_GCS.xyz",sep=""))
B <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=TRUE)
B.stat <- B$Stat
B.trans <- B$Trans

# Load data for the 11 Nov (C.):
my.data <- LoadPointCloud(Trans=A.trans,file=paste(Data.path,"/QT11nov_GCS.xyz",sep=""))
C <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=TRUE)
C.stat <- C$Stat
C.trans <- C$Trans

# Load data for the 20 Dec (D.):
my.data <- LoadPointCloud(Trans=A.trans,file=paste(Data.path,"/QT20dec_GCS.xyz",sep=""))
D <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=TRUE)
D.stat <- D$Stat
D.trans <- D$Trans

# Load data for the 11 Jan (E.):
my.data <- LoadPointCloud(Trans=A.trans,file=paste(Data.path,"/QT11jan_GCS.xyz",sep=""))
E <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=TRUE)
E.stat <- E$Stat
E.trans <- E$Trans

# Load data for the 15 Feb (FF.):
my.data <- LoadPointCloud(Trans=A.trans,file=paste(Data.path,"/QT15feb_GCS.xyz",sep=""))
FF <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=TRUE)
FF.stat <- FF$Stat
FF.trans <- FF$Trans

# Load data for the 8 Mar (G.):
my.data <- LoadPointCloud(Trans=A.trans,file=paste(Data.path,"/QT8mar_GCS.xyz",sep=""))
G <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=TRUE)
G.stat <- G$Stat
G.trans <- G$Trans

# Load data for the 26 Mar (H.):
my.data <- LoadPointCloud(Trans=A.trans,file=paste(Data.path,"/QT26mar_GCS.xyz",sep=""))
H <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=TRUE)
H.stat <- H$Stat
H.trans <- H$Trans

# Detrend ground surface ====
A.dim <- dim(A.stat$Min)
A <- data.frame(
 x=rep(A.stat$X,each=length(A.stat$X)),
 y=rep(A.stat$Y,A.dim[2]/length(A.stat$Y)),
 Min=as.vector(A.stat$Min))
Bs <- FitPlane(cbind(A$x[!is.na(A$Min)],A$y[!is.na(A$Min)],A$Min[!is.na(A$Min)]))
library(pracma)
ground <- -(A$x*Bs[1]+A$y*Bs[2]+Bs[4]+A$Min*Bs[3])
ground <- Reshape(ground,length(A.stat$X),length(A.stat$Y))

# Plot tranch of pointcloud ====
# Plot 3 section of point cloud :
#     - win1 => willow bush
#     - win2 => Black spruce tree
#     - win3 => dwarf birch bush

library(ggplot2)
win1.xlim <- c(471096-A.trans[1],471099-A.trans[1])
win1.ylim <- c(7203047.8-A.trans[2],7203047.9-A.trans[2])
win1 <- PtCl_RecTrunc(A,B,C,D,E,FF,G,H,xlim=win1.xlim,ylim=win1.ylim)
win2.xlim <- c(471094-A.trans[1],471096.5-A.trans[1])
win2.ylim <- c(7203050.7-A.trans[2],7203050.75-A.trans[2])
win2 <- PtCl_RecTrunc(A,B,C,D,E,FF,G,H,xlim=win2.xlim,ylim=win2.ylim)
win3.xlim <- c(471092.2-A.trans[1],471095.2-A.trans[1])
win3.ylim <- c(7203058.2-A.trans[2],7203058.35-A.trans[2])
win3 <- PtCl_RecTrunc(A,B,C,D,E,FF,G,H,xlim=win3.xlim,ylim=win3.ylim)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

w1 <- qplot(win1$x,win1$z,colour=win1$Date,alpha=I(.8),size = I(2),geom="point",
           xlim=c(win1.xlim[1]-.1,win1.xlim[2]+.1),ylim=c(-0.25,1.25),
           xlab="Distance (m)",ylab="Elevation (m)",
           main="Slice 10 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win1$x), max(win1$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win1$z), max(win1$z), by = 0.2),1))+
  coord_equal(ratio=1)+
  theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
w2 <- qplot(win2$x,win2$z,colour=win2$Date,alpha=I(0.8),size = I(2),geom="point",
           xlim=c(win2.xlim[1]-.1,win2.xlim[2]+.1),ylim=c(-0.25,3),
           xlab="Distance (m)",ylab="Elevation (m)",
           main="Slice 5 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win2$x), max(win2$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win2$z), max(win2$z), by = 0.2),1))+
  coord_equal(ratio=1)+scale_colour_manual(values=cbPalette)
w3 <- qplot(win3$x,win3$z,colour=win3$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win3.xlim[1]-.1,win3.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 15 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(from=min(win3$x), to=max(win3$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(from=min(win3$z), to=max(win3$z), by = 0.2),1))+
  coord_equal(ratio=1)+ theme(legend.position = "none")+scale_colour_manual(values=cbPalette)


ggsave("Tranch_1.pdf",plot=w1,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_2.pdf",plot=w2,scale=1,width=10,height=20,bg="transparent")
ggsave("Tranch_3.pdf",plot=w3,scale=1,width=10,height=6,bg="transparent")

# Preparation for WERC talk of the 18th January 2013 =====
# require to run previous section
# will plot and save profiles at getwd(). Profiles were used for WERC seminar presentation
getwd()

dev.off()
dd1 <- droplevels(subset(win1, Date=="23 Sept"))
dd2 <- droplevels(subset(win1, Date %in%  c("23 Sept", "23 Oct")))
dd3 <- droplevels(subset(win1, Date %in%  c("23 Sept","23 Oct", "11 Nov")))
dd4 <- droplevels(subset(win1, Date %in%  c("23 Sept", "23 Oct","11 Nov","20 Dec")))
dd5 <- droplevels(subset(win1, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan")))
dd6 <- droplevels(subset(win1, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan","15 Fev")))
dd7 <- droplevels(subset(win1, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan", "15 Fev","8 Mars")))
dd8 <- droplevels(subset(win1, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan", "15 Fev","8 Mars","26 Mars")))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
d1 <- qplot(dd1$x,dd1$z,colour=dd1$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win1.xlim[1]-.1,win1.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 10 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win1$x), max(win1$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win1$z), max(win1$z), by = 0.2),1))+
  coord_equal(ratio=1)+
  theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
d2 <- qplot(dd2$x,dd2$z,colour=dd2$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win1.xlim[1]-.1,win1.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 10 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win1$x), max(win1$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win1$z), max(win1$z), by = 0.2),1))+
  coord_equal(ratio=1)+
  theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
d3 <- qplot(dd3$x,dd3$z,colour=dd3$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win1.xlim[1]-.1,win1.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 10 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win1$x), max(win1$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win1$z), max(win1$z), by = 0.2),1))+
  coord_equal(ratio=1)+
  theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
d4 <- qplot(dd4$x,dd4$z,colour=dd4$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win1.xlim[1]-.1,win1.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 10 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win1$x), max(win1$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win1$z), max(win1$z), by = 0.2),1))+
  coord_equal(ratio=1)+
  theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
d5 <- qplot(dd5$x,dd5$z,colour=dd5$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win1.xlim[1]-.1,win1.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 10 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win1$x), max(win1$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win1$z), max(win1$z), by = 0.2),1))+
  coord_equal(ratio=1)+
  theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
d6 <- qplot(dd6$x,dd6$z,colour=dd6$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win1.xlim[1]-.1,win1.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 10 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win1$x), max(win1$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win1$z), max(win1$z), by = 0.2),1))+
  coord_equal(ratio=1)+
  theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
d7 <- qplot(dd7$x,dd7$z,colour=dd7$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win1.xlim[1]-.1,win1.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 10 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win1$x), max(win1$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win1$z), max(win1$z), by = 0.2),1))+
  coord_equal(ratio=1)+
  theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
d8 <- qplot(dd8$x,dd8$z,colour=win1$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win1.xlim[1]-.1,win1.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 10 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win1$x), max(win1$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win1$z), max(win1$z), by = 0.2),1))+
  coord_equal(ratio=1)+
  theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
ggsave("Tranch_1_1.pdf",plot=d1,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_1_2.pdf",plot=d2,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_1_3.pdf",plot=d3,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_1_4.pdf",plot=d4,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_1_5.pdf",plot=d5,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_1_6.pdf",plot=d6,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_1_7.pdf",plot=d7,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_1_8.pdf",plot=d8,scale=1,width=10,height=6,bg="transparent")

rm(d1,d2,d3,d4,d5,d6,d7,d8,dd1,dd2,dd3,dd4,dd5,dd6,dd7,dd8)

dev.off()
ee1 <- droplevels(subset(win2, Date=="23 Sept"))
ee2 <- droplevels(subset(win2, Date %in%  c("23 Sept", "23 Oct")))
ee3 <- droplevels(subset(win2, Date %in%  c("23 Sept","23 Oct", "11 Nov")))
ee4 <- droplevels(subset(win2, Date %in%  c("23 Sept", "23 Oct","11 Nov","20 Dec")))
ee5 <- droplevels(subset(win2, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan")))
ee6 <- droplevels(subset(win2, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan","15 Fev")))
ee7 <- droplevels(subset(win2, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan", "15 Fev","8 Mars")))
ee8 <- droplevels(subset(win2, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan", "15 Fev","8 Mars","26 Mars")))

e1 <- qplot(ee1$x,ee1$z,colour=ee1$Date,alpha=I(0.8),size = I(2),geom="point",
            xlim=c(win2.xlim[1]-.1,win2.xlim[2]+.1),ylim=c(-0.25,3),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 5 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win2$x), max(win2$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win2$z), max(win2$z), by = 0.2),1))+
  coord_equal(ratio=1)+scale_colour_manual(values=cbPalette)
e2 <- qplot(ee2$x,ee2$z,colour=ee2$Date,alpha=I(0.8),size = I(2),geom="point",
            xlim=c(win2.xlim[1]-.1,win2.xlim[2]+.1),ylim=c(-0.25,3),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 5 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win2$x), max(win2$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win2$z), max(win2$z), by = 0.2),1))+
  coord_equal(ratio=1)+scale_colour_manual(values=cbPalette)
e3 <- qplot(ee3$x,ee3$z,colour=ee3$Date,alpha=I(0.8),size = I(2),geom="point",
            xlim=c(win2.xlim[1]-.1,win2.xlim[2]+.1),ylim=c(-0.25,3),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 5 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win2$x), max(win2$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win2$z), max(win2$z), by = 0.2),1))+
  coord_equal(ratio=1)+scale_colour_manual(values=cbPalette)
e4 <- qplot(ee4$x,ee4$z,colour=ee4$Date,alpha=I(0.8),size = I(2),geom="point",
            xlim=c(win2.xlim[1]-.1,win2.xlim[2]+.1),ylim=c(-0.25,3),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 5 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win2$x), max(win2$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win2$z), max(win2$z), by = 0.2),1))+
  coord_equal(ratio=1)+scale_colour_manual(values=cbPalette)
e5 <- qplot(ee5$x,ee5$z,colour=ee5$Date,alpha=I(0.8),size = I(2),geom="point",
            xlim=c(win2.xlim[1]-.1,win2.xlim[2]+.1),ylim=c(-0.25,3),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 5 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win2$x), max(win2$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win2$z), max(win2$z), by = 0.2),1))+
  coord_equal(ratio=1)+scale_colour_manual(values=cbPalette)
e6 <- qplot(ee6$x,ee6$z,colour=ee6$Date,alpha=I(0.8),size = I(2),geom="point",
            xlim=c(win2.xlim[1]-.1,win2.xlim[2]+.1),ylim=c(-0.25,3),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 5 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win2$x), max(win2$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win2$z), max(win2$z), by = 0.2),1))+
  coord_equal(ratio=1)+scale_colour_manual(values=cbPalette)
e7 <- qplot(ee7$x,ee7$z,colour=ee7$Date,alpha=I(0.8),size = I(2),geom="point",
            xlim=c(win2.xlim[1]-.1,win2.xlim[2]+.1),ylim=c(-0.25,3),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 5 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win2$x), max(win2$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win2$z), max(win2$z), by = 0.2),1))+
  coord_equal(ratio=1)+scale_colour_manual(values=cbPalette)
e8 <- qplot(ee8$x,ee8$z,colour=ee8$Date,alpha=I(0.8),size = I(2),geom="point",
            xlim=c(win2.xlim[1]-.1,win2.xlim[2]+.1),ylim=c(-0.25,3),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 5 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win2$x), max(win2$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win2$z), max(win2$z), by = 0.2),1))+
  coord_equal(ratio=1)+scale_colour_manual(values=cbPalette)

ggsave("Tranch_2_1.pdf",plot=e1,scale=1,width=10,height=20,bg="transparent")
ggsave("Tranch_2_2.pdf",plot=e2,scale=1,width=10,height=20,bg="transparent")
ggsave("Tranch_2_3.pdf",plot=e3,scale=1,width=10,height=20,bg="transparent")
ggsave("Tranch_2_4.pdf",plot=e4,scale=1,width=10,height=20,bg="transparent")
ggsave("Tranch_2_5.pdf",plot=e5,scale=1,width=10,height=20,bg="transparent")
ggsave("Tranch_2_6.pdf",plot=e6,scale=1,width=10,height=20,bg="transparent")
ggsave("Tranch_2_7.pdf",plot=e7,scale=1,width=10,height=20,bg="transparent")
ggsave("Tranch_2_8.pdf",plot=e8,scale=1,width=10,height=20,bg="transparent")

rm(e1,e2,e3,e4,e5,e6,e7,e8,ee1,ee2,ee3,ee4,ee5,ee6,ee7,ee8)

dev.off()
ff1 <- droplevels(subset(win3, Date=="23 Sept"))
ff2 <- droplevels(subset(win3, Date %in%  c("23 Sept", "23 Oct")))
ff3 <- droplevels(subset(win3, Date %in%  c("23 Sept","23 Oct", "11 Nov")))
ff4 <- droplevels(subset(win3, Date %in%  c("23 Sept", "23 Oct","11 Nov","20 Dec")))
ff5 <- droplevels(subset(win3, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan")))
ff6 <- droplevels(subset(win3, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan","15 Fev")))
ff7 <- droplevels(subset(win3, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan", "15 Fev","8 Mars")))
ff8 <- droplevels(subset(win3, Date %in%  c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan", "15 Fev","8 Mars","26 Mars")))


f1 <- qplot(ff1$x,ff1$z,colour=ff1$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win3.xlim[1]-.1,win3.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 15 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(from=min(win3$x), to=max(win3$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(from=min(win3$z), to=max(win3$z), by = 0.2),1))+
  coord_equal(ratio=1)+ theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
f2 <- qplot(ff2$x,ff2$z,colour=ff2$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win3.xlim[1]-.1,win3.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 15 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(from=min(win3$x), to=max(win3$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(from=min(win3$z), to=max(win3$z), by = 0.2),1))+
  coord_equal(ratio=1)+ theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
f3 <- qplot(ff3$x,ff3$z,colour=ff3$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win3.xlim[1]-.1,win3.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 15 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(from=min(win3$x), to=max(win3$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(from=min(win3$z), to=max(win3$z), by = 0.2),1))+
  coord_equal(ratio=1)+ theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
f4 <- qplot(ff4$x,ff4$z,colour=ff4$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win3.xlim[1]-.1,win3.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 15 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(from=min(win3$x), to=max(win3$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(from=min(win3$z), to=max(win3$z), by = 0.2),1))+
  coord_equal(ratio=1)+ theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
f5 <- qplot(ff5$x,ff5$z,colour=ff5$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win3.xlim[1]-.1,win3.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 15 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(from=min(win3$x), to=max(win3$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(from=min(win3$z), to=max(win3$z), by = 0.2),1))+
  coord_equal(ratio=1)+ theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
f6 <- qplot(ff6$x,ff6$z,colour=ff6$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win3.xlim[1]-.1,win3.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 15 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(from=min(win3$x), to=max(win3$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(from=min(win3$z), to=max(win3$z), by = 0.2),1))+
  coord_equal(ratio=1)+ theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
f7 <- qplot(ff7$x,ff7$z,colour=ff7$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win3.xlim[1]-.1,win3.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 15 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(from=min(win3$x), to=max(win3$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(from=min(win3$z), to=max(win3$z), by = 0.2),1))+
  coord_equal(ratio=1)+ theme(legend.position = "none")+scale_colour_manual(values=cbPalette)
f8 <- qplot(ff8$x,ff8$z,colour=ff8$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win3.xlim[1]-.1,win3.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 15 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(from=min(win3$x), to=max(win3$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(from=min(win3$z), to=max(win3$z), by = 0.2),1))+
  coord_equal(ratio=1)+ theme(legend.position = "none")+scale_colour_manual(values=cbPalette)

ggsave("Tranch_3_1.pdf",plot=f1,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_3_2.pdf",plot=f2,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_3_3.pdf",plot=f3,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_3_4.pdf",plot=f4,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_3_5.pdf",plot=f5,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_3_6.pdf",plot=f6,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_3_7.pdf",plot=f7,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_3_8.pdf",plot=f8,scale=1,width=10,height=6,bg="transparent")

rm(ff1,ff2,ff3,ff4,ff5,ff6,ff7,ff8,f1,f2,f3,f4,f5,f6,f7,f8)

#Exploring data by Plotting ====

H.points <- H.stat$Density*dx*dy
A.points <- A.stat$Density*dx*dy
Snow.depth <- H.stat$Min-A.stat$Min
Snow.depth[H.points<10 | A.points<20] <- NA

dev.off()
par(mar=c(5,4.5,4,7))

image(image.smooth(Snow.depth),zlim=c(0.2,0.9), col=grey(seq(0,1,length=64)),axes=F,main="Snow depth map the 26 March (m)")
axis(1,at=seq(0,250,50),labels=paste(seq(0,12.5,length=6)))
axis(2,at=seq(0,250,50),labels=paste(seq(0,12.5,length=6)))
image.plot(image.smooth(Snow.depth), legend.only=T, main="Snow depth map the 26 March (m)",zlim=c(0.2,0.9), col=grey(seq(0,1,length=64)))
hist(as.vector(Snow.depth),breaks=100, xlab="Snow depth (m)", main="Snow depth distribution")

# Plot difference of maximum values
AB.max <- B.stat$Max-A.stat$Max
BC.max <- C.stat$Max-B.stat$Max
CD.max <- D.stat$Max-C.stat$Max
DE.max <- E.stat$Max-D.stat$Max
EF.max <- FF.stat$Max-E.stat$Max
FG.max <- G.stat$Max-FF.stat$Max
GH.max <- H.stat$Max-G.stat$Max
Veg.thik <- A.stat$Max-A.stat$Min
Veg.thik[A.points<50] <- NA

require(fields)
my.zlim <- c(-.5,.5)

par(mfrow=c(2,2),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
image.plot(AB.max,nlevel=64,zlim=my.zlim,main='23 Sept - 23 Oct',cex.lab=1.1)
image.plot(BC.max,nlevel=64,zlim=my.zlim,main='23 Oct - 11 Nov',cex.lab=1.1)
image.plot(CD.max,nlevel=64,zlim=my.zlim,main='11 Nov - 20 Dec',cex.lab=1.1)
image.plot(DE.max,nlevel=64,zlim=my.zlim,main='20 Dec - 11 Jan',cex.lab=1.1)
par(mfrow=c(2,2),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
image.plot(EF.max,nlevel=64,zlim=my.zlim,main='11 Jan - 15 Feb',cex.lab=1.1)
image.plot(FG.max,nlevel=64,zlim=my.zlim,main='15 Feb - 8 Mar',cex.lab=1.1)
image.plot(GH.max,nlevel=64,zlim=my.zlim,main='8 Mar - 26 Mar',cex.lab=1.1)
image.plot(Veg.thik,zlim=c(0,4),main='Veg thickness',cex.lab=1.1)
dev.off()

par(mfrow=c(2,2),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
hist(as.vector(AB.max),200,xlim=my.zlim,main='23 Sept - 23 Oct',cex.lab=1.1)
hist(as.vector(BC.max),200,xlim=my.zlim,main='23 Oct - 11 Nov',cex.lab=1.1)
hist(as.vector(CD.max),200,xlim=my.zlim,main='11 Nov - 20 Dec',cex.lab=1.1)
hist(as.vector(DE.max),200,xlim=my.zlim,main='20 Dec - 11 Jan',cex.lab=1.1)
par(mfrow=c(2,2),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
hist(as.vector(EF.max),200,xlim=my.zlim,main='11 Jan - 15 Feb',cex.lab=1.1)
hist(as.vector(FG.max),200,xlim=my.zlim,main='15 Feb - 8 Mar',cex.lab=1.1)
hist(as.vector(GH.max),200,xlim=my.zlim,main='8 Mar - 26 Mar',cex.lab=1.1)
hist((as.vector(Veg.thik)),200,xlim=c(0,4),main='Vegetation Thickness',cex.lab=1.1)

require(abind)
my.max <- abind(AB.max ,BC.max ,CD.max ,DE.max,EF.max,FG.max,GH.max,along=3)
my.min <- abind(
  B.stat$Min-A.stat$Min,
  C.stat$Min-B.stat$Min,
  D.stat$Min-C.stat$Min,
  E.stat$Min-D.stat$Min,
  FF.stat$Min-E.stat$Min,
  G.stat$Min-FF.stat$Min,
  H.stat$Min-G.stat$Min,
  along=3)
my.snowdepth <- abind(
  B.stat$Min-A.stat$Min,
  C.stat$Min-A.stat$Min,
  D.stat$Min-A.stat$Min,
  E.stat$Min-A.stat$Min,
  FF.stat$Min-A.stat$Min,
  G.stat$Min-A.stat$Min,
  H.stat$Min-A.stat$Min,
  along=3)

my.zlim=c(0,1)
for(i in 1:7){  
  par(mfrow=c(2,2),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
  image.plot(my.snowdepth[,,i],zlim=my.zlim,main=paste('Snowdepth ',as.character(i)))
  contour(my.snowdepth[,,i],add=T,levels = pretty(my.zlim, 10))
  hist(as.vector(my.snowdepth[,,i]),200)
  plot(my.snowdepth[,,i],ground)
  image.plot(Veg.thik,zlim=c(0,2),main='Veg thickness',cex.lab=1.1)
  contour(Veg.thik,add=T)
}

my.zlim=c(-0.4,0.4)
for(i in 1:7){  
  par(mfrow=c(2,1),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
  image.plot(my.min[,,i],zlim=my.zlim,main=paste('Snow accumulation ',as.character(i)))
  contour(my.min[,,i],add=T,levels = pretty(my.zlim, 10))
  plot(my.min[,,i],ground)
  #image.plot(Veg.thik,zlim=c(0,2),main='Veg thickness',cex.lab=1.1)
  #contour(Veg.thik,add=T)
}

image.plot(Veg.thik,main='Vegetation Thickness ')
par(mar=c(5,4.5,4,7))
image(ground, axes=F, col=bwr.colors(64), zlim=c(-0.4,0.4))
axis( 1, at =seq(0,1,0.2),labels=c(paste(seq(0,12.5,length=6))))
axis(2, at =seq(0,1,0.2),labels=c(paste(seq(0,12.5,length=6))))
image.plot(ground,legend.only=T, legend.mar=2,col=bwr.colors(64), zlim=c(-0.4,0.4))

axis(side = 2, at = c(1,3,7,10))

par(mfrow=c(1,1),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
image.plot(log10(A.points))

qplot(as.vector(Veg.thik),as.vector(ground),alpha=I(1/10))
qplot(as.vector(Veg.thik),as.vector(my.snowdepth[,,1]),alpha=I(1/10),size = I(5),ylim=my.zlim,xlim=my.zlim)
qplot(as.vector(Veg.thik),as.vector(my.snowdepth[,,2]),alpha=I(1/10),size = I(5),ylim=my.zlim,xlim=my.zlim)
qplot(as.vector(Veg.thik),as.vector(my.snowdepth[,,3]),alpha=I(1/10),size = I(5),ylim=my.zlim,xlim=my.zlim)
qplot(as.vector(Veg.thik),as.vector(my.snowdepth[,,4]),alpha=I(1/10),size = I(5),ylim=my.zlim,xlim=my.zlim)
qplot(as.vector(Veg.thik),as.vector(my.snowdepth[,,5]),alpha=I(1/10),size = I(5),ylim=my.zlim,xlim=my.zlim)
qplot(as.vector(Veg.thik),as.vector(my.snowdepth[,,6]),alpha=I(1/10),size = I(5),ylim=my.zlim,xlim=my.zlim)
qplot(as.vector(Veg.thik),as.vector(my.snowdepth[,,7]),alpha=I(1/10),size = I(5),ylim=my.zlim,xlim=my.zlim)


image.plot(ground,zlim=c(-.5,.5),main='Ground Residual',cex.lab=1.1)
contour(ground,add=T)

## To complete !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

my.surface <- abind(
  A.stat$Min+(ground-A.stat$Min)-mean(A.stat$Min+(ground-A.stat$Min),na.rm=TRUE),
  B.stat$Min+(ground-A.stat$Min)-mean(B.stat$Min+(ground-A.stat$Min),na.rm=TRUE),
  C.stat$Min+(ground-A.stat$Min)-mean(C.stat$Min+(ground-A.stat$Min),na.rm=TRUE),
  D.stat$Min+(ground-A.stat$Min)-mean(D.stat$Min+(ground-A.stat$Min),na.rm=TRUE),
  E.stat$Min+(ground-A.stat$Min)-mean(E.stat$Min+(ground-A.stat$Min),na.rm=TRUE),
  FF.stat$Min+(ground-A.stat$Min)-mean(FF.stat$Min+(ground-A.stat$Min),na.rm=TRUE),
  G.stat$Min+(ground-A.stat$Min)-mean(G.stat$Min+(ground-A.stat$Min),na.rm=TRUE),
  H.stat$Min+(ground-A.stat$Min)-mean(H.stat$Min+(ground-A.stat$Min),na.rm=TRUE),
  along=3)

par(mfrow=c(1,1),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
bwr.colors <- colorRampPalette(c("blue","white","red"))



image.plot(my.surface[,,5] -my.surface[,,6],zlim=c(-0.1,0.1),col=bwr.colors(64))
image.plot(my.surface[,,5] -my.surface[,,7],zlim=c(-0.1,0.1),col=bwr.colors(64))
image.plot(my.surface[,,6] -my.surface[,,7],zlim=c(-0.1,0.1),col=bwr.colors(64))
plot((ground),my.surface[,,5] -my.surface[,,7],ylim=c(-0.5,0.5))

my.diff <- abind(
  my.surface[,,1] -my.surface[,,8],
  my.surface[,,2] -my.surface[,,8],
  my.surface[,,3] -my.surface[,,8],
  my.surface[,,4] -my.surface[,,8],
  my.surface[,,5] -my.surface[,,8],
  my.surface[,,6] -my.surface[,,8],
  my.surface[,,7]- my.surface[,,8],
  along=3
)
par(mfrow=c(3,3),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
image.plot(ground,legend.only=T, legend.mar=0,col=bwr.colors(64), zlim=c(-0.4,0.4))
image.plot(image.smooth(my.surface[,,1]),zlim=c(-0.2,0.2),col=bwr.colors(64), main="Ground surface 23 Sept")
image.plot(image.smooth(my.surface[,,2]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Snow surface 23 Oct")
image.plot(image.smooth(my.surface[,,3]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Snow surface 11 Nov")
image.plot(image.smooth(my.surface[,,4]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Snow surface 20 Dec")
image.plot(image.smooth(my.surface[,,5]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Snow surface 11 Jan")
image.plot(image.smooth(my.surface[,,6]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Snow surface 15 Feb")
image.plot(image.smooth(my.surface[,,7]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Snow surface 8 Mar")
image.plot(image.smooth(my.surface[,,8]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Snow surface 26 Mar")
image.plot((A.stat$Max+(ground-A.stat$Min)-mean(A.stat$Max+(ground-A.stat$Min),na.rm=TRUE)),zlim=c(-0.6,1.5),col=bwr.colors(64),main="Canopy height")


# plotting snow depth over time
library(colorRamps)
z.lim <- c(0,1)
par(mfrow=c(3,3),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
image.plot(image.smooth(my.snowdepth[,,1]),zlim=z.lim,col=matlab.like(32), main="Snow depth 23 Oct")
image.plot(image.smooth(my.snowdepth[,,2]),zlim=z.lim,col=matlab.like(32),main="Snow depth 11 Nov")
image.plot(image.smooth(my.snowdepth[,,3]),zlim=z.lim,col=matlab.like(32),main="Snow depth 20 Dec")
image.plot(image.smooth(my.snowdepth[,,4]),zlim=z.lim,col=matlab.like(32),main="Snow depth 11 Jan")
image.plot(image.smooth(my.snowdepth[,,5]),zlim=z.lim,col=matlab.like(32),main="Snow surface 11 Jan")
image.plot(image.smooth(my.snowdepth[,,6]),zlim=z.lim,col=matlab.like(32),main="Snow surface 15 Feb")
image.plot(image.smooth(my.snowdepth[,,7]),zlim=z.lim,col=matlab.like(32),main="Snow surface 8 Mar")
image.plot((A.stat$Max+(ground-A.stat$Min)-mean(A.stat$Max+(ground-A.stat$Min),na.rm=TRUE)),zlim=c(-0.6,1.5),col=bwr.colors(64),main="Canopy height")





par(mfrow=c(1,1),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
a <- diff(image.smooth(my.surface[,,1]),my.surface[,,1])
image.plot(image.smooth(my.surface[,,1])-my.surface[,,1],zlim=c(-0.2,0.2),col=bwr.colors(64), main="Min surf 23 Sept")



hist(Veg.thik,breaks=100)
image.plot(my.diff[,,7],zlim=c(-0.1,0.1),col=bwr.colors(64))

# Plot Thickness of point cloud for each date
par(mfrow=c(3,3),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
image.plot((A.stat$Max-A.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 23 Sept (m)")
image.plot((B.stat$Max-B.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 23 Oct (m)")
image.plot((C.stat$Max-C.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 11 Nov (m)")
image.plot((D.stat$Max-D.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 20 Dec (m)")
image.plot((E.stat$Max-E.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 11 Jan (m)")
image.plot((FF.stat$Max-FF.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 15 Feb (m)")
image.plot((G.stat$Max-G.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 8 Mar (m)")
image.plot((H.stat$Max-H.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 26 Mar (m)")

# plot distribution of point cloud thickness as boxplot
par(mfrow=c(1,1),oma = c( 1.5, 1.5, 1.5,1.5 ),mar=c(2,4.5,2,2))
boxplot(data.frame( Sept.23=as.vector(((A.stat$Max-A.stat$Min)+0.01)),
                    Oct.23=as.vector(((B.stat$Max-B.stat$Min)+0.01)),
                    Nov.11=as.vector(((C.stat$Max-C.stat$Min)+0.01)),
                    Dec.20=as.vector(((D.stat$Max-D.stat$Min)+0.01)),
                    Jan.11=as.vector(((E.stat$Max-E.stat$Min)+0.01)),
                    Fev.15=as.vector(((FF.stat$Max-FF.stat$Min)+0.01)),
                    Mar.8=as.vector(((G.stat$Max-G.stat$Min)+0.01)),
                    Mar.26=as.vector(((H.stat$Max-H.stat$Min)+0.01))), 
        log="y", main="Thickness of point cloud",
        ylab="Thickness (m)",cex.lab=1.5,cex.axis=1.5
)

# Plot snow depth distribution as box plot
par(mfrow=c(1,1),oma = c( 1, 1, 1,1 ),mar=c(2,4,2,1.5))
boxplot(data.frame( 
  Oct.23=as.vector(my.snowdepth[,,1]),
  Nov.11=as.vector(my.snowdepth[,,2]),
  Dec.20=as.vector(my.snowdepth[,,3]),
  Jan.11=as.vector(my.snowdepth[,,4]),
  Fev.15=as.vector(my.snowdepth[,,5]),
  Mar.8=as.vector(my.snowdepth[,,6]),
  Mar.26=as.vector(my.snowdepth[,,7])), 
        main="Snow depth",
        ylab="Thickness (m)"
)

# Plot Canopy height vs final snow surface 
par(mfrow=c(1,1),oma = c( 1, 1, 1,1 ),mar=c(4,4,1.5,1.5))
a <- data.frame(
  aa = as.vector(A.stat$Max+(ground-A.stat$Min)-mean(A.stat$Max+(ground-A.stat$Min),na.rm=TRUE)),
  bb= as.vector(H.stat$Min+(ground-A.stat$Min)-mean(H.stat$Min+(ground-A.stat$Min),na.rm=TRUE)))
 

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
qplot(x=a$aa,
      y=a$bb,
      ylim=c(-0.2,0.2),xlim=c(-.5,2),
      xlab="Canopy height (m)",
      ylab="26 March snow surface variation (m)" , alpha=I(0.05))+
theme(axis.text.x  = element_text(size=16),
      axis.text.y  = element_text(size=16),
      axis.title.x= element_text(size=20),
      axis.title.y  = element_text(size=20))
rsq.range <- seq(-0.30,0.5, 0.02)
rsq.record <- rep(0, times=length(rsq.range))
k <- 1
for (i in rsq.range){
fit1 <- lm(a$bb[a$aa<i]~a$aa[a$aa<i])
rsq.record[k] <- summary(fit1)$r.squared 
k <- k+1
}
qplot(rsq.range, rsq.record,xlab="Range of data", ylab="R square value")+
  theme(axis.text.x  = element_text(size=16),
        axis.text.y  = element_text(size=16),
        axis.title.x= element_text(size=20),
        axis.title.y  = element_text(size=20))

# Change in maximum elevation between 23 Sept and 26 March
par(mfrow=c(3,3),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
image.plot(image.smooth((B.stat$Max)-(A.stat$Max)),zlim=c(-.5,.5),col=bwr.colors(64))
image.plot(image.smooth((C.stat$Max)-(A.stat$Max)),zlim=c(-.5,.5),col=bwr.colors(64))
image.plot(image.smooth((D.stat$Max)-(A.stat$Max)),zlim=c(-.5,.5),col=bwr.colors(64))
image.plot(image.smooth((E.stat$Max)-(A.stat$Max)),zlim=c(-.5,.5),col=bwr.colors(64))
image.plot(image.smooth((FF.stat$Max)-(A.stat$Max)),zlim=c(-.5,.5),col=bwr.colors(64))
image.plot(image.smooth((G.stat$Max)-(A.stat$Max)),zlim=c(-.5,.5),col=bwr.colors(64))
image.plot(image.smooth((H.stat$Max)-(A.stat$Max)),zlim=c(-.5,.5),col=bwr.colors(64))
image.plot((A.stat$Max+(ground-A.stat$Min)-min(A.stat$Max+(ground-A.stat$Min),na.rm=TRUE)),zlim=c(0,1.5),col=grey(seq(0,1,length=64)),main="Canopy height")

par(mfrow=c(1,1),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
boxplot(as.vector((B.stat$Max)-(A.stat$Max)),
        as.vector((C.stat$Max)-(A.stat$Max)),
        as.vector((D.stat$Max)-(A.stat$Max)),
        as.vector((E.stat$Max)-(A.stat$Max)),
        as.vector((FF.stat$Max)-(A.stat$Max)),
        as.vector((G.stat$Max)-(A.stat$Max)),
        as.vector((H.stat$Max)-(A.stat$Max)))





# test
Mar.26 <- data.frame(
  Canop.orig = as.vector(A.stat$Max+(ground-A.stat$Min)-min(A.stat$Max+(ground-A.stat$Min),na.rm=TRUE)),
  Snow.surf= as.vector(H.stat$Min+(ground-A.stat$Min)-mean(H.stat$Min+(ground-A.stat$Min),na.rm=TRUE)),
  Veg_thick.orig=as.vector((A.stat$Max-A.stat$Min))
  )

ggplot(Mar.26,aes(Canop.orig,Snow.surf,colour=(Veg_thick.orig)),xlim=c(-0.3,1))+
  geom_point()+
 scale_color_gradient(limits=c(0,.4),na.value="transparent")

qplot(Snow.surf,Canop.orig,data=Mar.26,xlim=c(-0.5,0.5),ylim=c(0,2))

ggplot(Mar.26,aes(x=Veg_thick.orig))+geom_histogram(binwidth=.1)
#TEST ZONE: =====

# 
# setwd("/Users/simonfilhol/Desktop")
# library(R.matlab)
# writeMat("SnowDepth.mat",Depth=Snow.depth)
# writeMat("Ground_d.mat",Ground=ground)
# 







