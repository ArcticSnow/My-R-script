# 13 November 2012, Simon Filhol,
# 
# Script to derive snow depth map from LiDAR winter 2011/2011 at Glenn Creek
# 
# MAKE SURE TO LOAD FUNCTIONs:
#====

ifelse({Sys.info()['sysname']=="Windows"},{
  setwd("G:/GlennCreek/Grd_LiDAR/Glenn_Creek1112/UTM/subselection_2/Clean")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/FitPlane.r")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/Basic_statmap_ptcloud.r")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/LoadPointCloud.r")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/Multiplot.r")
},{
setwd("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Winter 11_12/Lidar Survey/UTM all/subselection_2/Clean")
 source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/Basic_statmap_ptcloud.r")
 source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/LoadPointCloud.r")
 source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/Multiplot.R")
 source("/Users/simonfilhol/github/local/My_R_script/Function/FitPlane.R")
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
dx <- 0.1
dy <- 0.1

# Load data for the 23 Sept (A.):
my.data <- LoadPointCloud()
A <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=T)
A.stat <- A$Stat
A.trans <- A$Trans


# Load data for the 23 Oct (B.):
my.data <- LoadPointCloud(Trans=A.trans)
B <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=T)
B.stat <- B$Stat
B.trans <- B$Trans

# Load data for the 11 Nov (C.):
my.data <- LoadPointCloud(Trans=A.trans)
C <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=T)
C.stat <- C$Stat
C.trans <- C$Trans

# Load data for the 20 Dec (D.):
my.data <- LoadPointCloud(Trans=A.trans)
D <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=T)
D.stat <- D$Stat
D.trans <- D$Trans

# Load data for the 11 Jan (E.):
my.data <- LoadPointCloud(Trans=A.trans)
E <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=T)
E.stat <- E$Stat
E.trans <- E$Trans

# Load data for the 15 Feb (FF.):
my.data <- LoadPointCloud(Trans=A.trans)
FF <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=T)
FF.stat <- FF$Stat
FF.trans <- FF$Trans

# Load data for the 8 Mar (G.):
my.data <- LoadPointCloud(Trans=A.trans)
G <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=T)
G.stat <- G$Stat
G.trans <- G$Trans

# Load data for the 26 Mar (H.):
my.data <- LoadPointCloud(Trans=A.trans)
H <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=T)
H.stat <- H$Stat
H.trans <- H$Trans


# 
#Exploring data by Plotting ====

H.points <- H.stat$Density*dx*dy
A.points <- A.stat$Density*dx*dy
Snow.depth <- H.stat$Min-A.stat$Min
Snow.depth[H.points<10 | A.points<20] <- NA

# Detrend ground surface
A.dim <- dim(A.stat$Min)
A <- data.frame(
 x=rep(A.stat$X,each=length(A.stat$X)),
 y=rep(A.stat$Y,A.dim[2]/length(A.stat$Y)),
 Min=as.vector(A.stat$Min))
Bs <- FitPlane(cbind(A$x[!is.na(A$Min)],A$y[!is.na(A$Min)],A$Min[!is.na(A$Min)]))
library(pracma)
ground <- -(A$x*Bs[1]+A$y*Bs[2]+Bs[4]+A$Min*Bs[3])
ground <- Reshape(ground,length(A.stat$X),length(A.stat$Y))


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

par(mfrow=c(3,3),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
for(i in 1:7){  
  my.zlim=c(mean(as.vector(my.min[,,i]), na.rm=TRUE)-2*sd(as.vector(my.min[,,i]), na.rm=TRUE),
            mean(as.vector(my.min[,,i]), na.rm=TRUE)+2*sd(as.vector(my.min[,,i]), na.rm=TRUE))
  image.plot(my.min[,,i],zlim=my.zlim,main=paste('Snow accumulation ',as.character(i)))
  #contour(my.min[,,i],add=T,levels = pretty(-1:1, n=20))
}
image.plot(Veg.thik,main='Vegetation Thickness ')
image.plot(ground,main='Ground ')

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

# Plot tranch of pointcloud ====
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
w3 <- qplot(win3$x,win3$z,colour=win3$Date,alpha=I(.8),size = I(2),geom="point",
            xlim=c(win3.xlim[1]-.1,win3.xlim[2]+.1),ylim=c(-0.25,1.25),
            xlab="Distance (m)",ylab="Elevation (m)",
            main="Slice 15 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(from=min(win3$x), to=max(win3$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(from=min(win3$z), to=max(win3$z), by = 0.2),1))+
  coord_equal(ratio=1)+ theme(legend.position = "none")


w1 <- qplot(win1$x,win1$z,colour=win1$Date,alpha=I(.8),size = I(2),geom="point",
           xlim=c(win1.xlim[1]-.1,win1.xlim[2]+.1),ylim=c(-0.25,1.25),
           xlab="Distance (m)",ylab="Elevation (m)",
           main="Slice 10 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win1$x), max(win1$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win1$z), max(win1$z), by = 0.2),1))+
  coord_equal(ratio=1)+
  theme(legend.position = "none")
w2 <- qplot(win2$x,win2$z,colour=win2$Date,alpha=I(0.8),size = I(2),geom="point",
           xlim=c(win2.xlim[1]-.1,win2.xlim[2]+.1),ylim=c(-0.25,3),
           xlab="Distance (m)",ylab="Elevation (m)",
           main="Slice 5 cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win2$x), max(win2$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win2$z), max(win2$z), by = 0.2),1))+
  coord_equal(ratio=1)



ggsave("Tranch_1.pdf",plot=w1,scale=1,width=10,height=6,bg="transparent")
ggsave("Tranch_2.pdf",plot=w2,scale=1,width=10,height=20,bg="transparent")
ggsave("Tranch_3.pdf",plot=w3,scale=1,width=10,height=6,bg="transparent")

#TEST ZONE: =====

# 
# setwd("/Users/simonfilhol/Desktop")
# library(R.matlab)
# writeMat("SnowDepth.mat",Depth=Snow.depth)
# writeMat("Ground_d.mat",Ground=ground)
# 


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
image.plot(image.smooth(my.surface[,,1]),zlim=c(-0.2,0.2),col=bwr.colors(64), main="Min surf 23 Sept")
image.plot(image.smooth(my.surface[,,2]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Min surf 23 Oct")
image.plot(image.smooth(my.surface[,,3]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Min surf 11 Nov")
image.plot(image.smooth(my.surface[,,4]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Min surf 20 Dec")
image.plot(image.smooth(my.surface[,,5]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Min surf 11 Jan")
image.plot(image.smooth(my.surface[,,6]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Min surf 15 Feb")
image.plot(image.smooth(my.surface[,,7]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Min surf 8 Mar")
image.plot(image.smooth(my.surface[,,8]),zlim=c(-0.2,0.2),col=bwr.colors(64),main="Min surf 26 Mar")
image.plot(image.smooth((Veg.thik)),zlim=c(0,1),col=grey(seq(from=0,to=1,by=1/64)), main="Veg thickness")

par(mfrow=c(1,1),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
a <- diff(image.smooth(my.surface[,,1]),my.surface[,,1])
image.plot(image.smooth(my.surface[,,1])-my.surface[,,1],zlim=c(-0.2,0.2),col=bwr.colors(64), main="Min surf 23 Sept")



hist(Veg.thik,breaks=100)
image.plot(my.diff[,,7],zlim=c(-0.1,0.1),col=bwr.colors(64))


par(mfrow=c(3,3),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
image.plot((A.stat$Max-A.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 23 Sept (m)")
image.plot((B.stat$Max-B.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 23 Oct (m)")
image.plot((C.stat$Max-C.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 11 Nov (m)")
image.plot((D.stat$Max-D.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 20 Dec (m)")
image.plot((E.stat$Max-E.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 11 Jan (m)")
image.plot((FF.stat$Max-FF.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 15 Feb (m)")
image.plot((G.stat$Max-G.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 8 Mar (m)")
image.plot((H.stat$Max-H.stat$Min),zlim=c(0,1),col=grey(seq(1, 0, length=64)), main="Cloud Thickness 26 Mar (m)")

par(mfrow=c(1,1),oma = c( 1, 1, 1,1 ),mar=c(1.5,4,1.5,1.5))
boxplot(data.frame( Sept.23=as.vector(((A.stat$Max-A.stat$Min)+0.01)),
        Oct.23=as.vector(((B.stat$Max-B.stat$Min)+0.01)),
        Nov.11=as.vector(((C.stat$Max-C.stat$Min)+0.01)),
        Dec.20=as.vector(((D.stat$Max-D.stat$Min)+0.01)),
        Jan.11=as.vector(((E.stat$Max-E.stat$Min)+0.01)),
        Fev.15=as.vector(((FF.stat$Max-FF.stat$Min)+0.01)),
        Mar.8=as.vector(((G.stat$Max-G.stat$Min)+0.01)),
        Mar.26=as.vector(((H.stat$Max-H.stat$Min)+0.01))), 
        log="y", main="Thickness of point cloud",
        ylab="Thickness (m)"
        )


hist(as.vector((log10(H.stat$Max-H.stat$Min))),breaks=100)







