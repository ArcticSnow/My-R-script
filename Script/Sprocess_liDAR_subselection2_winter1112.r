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
setwd("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Winter 11_12/Lidar Survey/UTM all/subselection_2")
# source()
# source()
# source()
# source()
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
xlim <-c(1,11)
ylim <- c(1,11)
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
EF.trans <- FF$Trans

# Load data for the 8 Mar (G.):
my.data <- LoadPointCloud(Trans=A.trans)
G <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=T)
G.stat <- G$Stat
G.trans <- G$Trans

# Load data for the 11 Jan (H.):
my.data <- LoadPointCloud(Trans=A.trans)
H <- Data.prepare(my.data,xlim,ylim,dx,dy,return.xyz=T)
H.stat <- H$Stat
H.trans <- H$Trans


# 
#Exploring data by Plotting ====

H.points <- H.stat$Density*dx*dy
A.points <- A.stat$Density*dx*dy
Snow.depth <- H.stat$Min-A.stat$Min
Snow.depth[H.points<10 | A.points<100] <- NA
image(Snow.depth,zlim=c(0,1))
contour(Snow.depth,add=T)

plot(Snow.depth,A.points)


A.dim <- dim(A.stat$Min)
A <- data.frame(
 x=rep(A.stat$X,each=length(A.stat$X)),
 y=rep(A.stat$Y,A.dim[2]/length(A.stat$Y)),
 Min=as.vector(A.stat$Min))

Bs <- FitPlane(cbind(A$x[!is.na(A$Min)],A$y[!is.na(A$Min)],A$Min[!is.na(A$Min)]))
library(pracma)
ground <- -(A$x*Bs[1]+A$y*Bs[2]+Bs[4]+A$Min*Bs[3])
ground <- Reshape(ground,length(A.stat$X),length(A.stat$Y))

plot(ground,Snow.depth)

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
  plot(my.snowdepth[,,i],Veg.thik,ylim=my.zlim,xlim=my.zlim)
  image.plot(Veg.thik,zlim=c(0,2),main='Veg thickness',cex.lab=1.1)
  contour(Veg.thik,add=T)
}

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
win.xlim <- c(471096-A.trans[1],471099-A.trans[1])
win.ylim <- c(7203047.8-A.trans[2],7203047.9-A.trans[2])

win.xyz.A <- A$xyz[A$xyz[,1]>win.xlim[1] & A$xyz[,1]<win.xlim[2],] 
win.xyz.A <- win.xyz.A[win.xyz.A[,2]>win.ylim[1] & win.xyz.A[,2]<win.ylim[2],]

 
win.xyz.G <- G$xyz[G$xyz[,1]>win.xlim[1] & G$xyz[,1]<win.xlim[2],] 
win.xyz.G <- win.xyz.G[win.xyz.G[,2]>win.ylim[1] & win.xyz.G[,2]<win.ylim[2],]

win.xyz.H <- H$xyz[H$xyz[,1]>win.xlim[1] & H$xyz[,1]<win.xlim[2],] 
win.xyz.H <-  win.xyz.H[win.xyz.H[,2]>win.ylim[1] & win.xyz.H[,2]<win.ylim[2],]





win <- data.frame(
  Date=as.factor(c(rep("23 Sept",times=dim(win.xyz.A)[1]),
                   rep("8 Mars",times=dim(win.xyz.G)[1]),
                   rep('26 Mars',times=dim(win.xyz.H)[1]))),
  x=c(win.xyz.A[,1],win.xyz.G[,1],win.xyz.H[,1]),
  z=c(win.xyz.A[,3],win.xyz.G[,3],win.xyz.H[,3]))

qplot(win$x,win$z,colour=win$Date,alpha=I(1/4),size = I(4),
      asp=1.5/3.2,xlim=c(win.xlim[1]-.1,win.xlim[2]+.1),ylim=c(-0.25,1.25))

qplot(win.xyz[,1],win.xyz[,3],alpha=I(1/10),size = I(5))

#TEST ZONE: =====

# 
# setwd("/Users/simonfilhol/Desktop")
# library(R.matlab)
# writeMat("SnowDepth.mat",Depth=Snow.depth)
# writeMat("Ground_d.mat",Ground=ground)
# 


Canopy <- A.stat$Max-A.stat$Min
#Canopy[Canopy>1.5] <- NA
image(Canopy)
contour(Canopy,add=T)
hist(Canopy,100)









