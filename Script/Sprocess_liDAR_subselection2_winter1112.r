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

Data.prepare <- function(my.data,xlim,ylim,dx,dy){
  xyz <- my.data$Data
  translation <- my.data$Translation
  rm(my.data)
  
  xyz <- xyz[xyz[,1]>=xlim[1] & xyz[,1] <=xlim[2],]
  xyz <- xyz[xyz[,2]>=ylim[1] & xyz[,2] <=ylim[2],]
  
  stat <- Basic.statmap.ptcloud(xyz,dx,dy,xlim,ylim)
  rm(xyz)
  All <- list(Stat= stat,
              Trans=translation)
  return(All)
}
#====
# Boundary of area of interest (xlim,ylim), and resolution fo final product (dx,dy)
xlim <-c(1,11)
ylim <- c(1,11)
dx <- 0.1
dy <- 0.1

# Load data for the 23 Sept (A.):
my.data <- LoadPointCloud()
A <- Data.prepare(my.data,xlim,ylim,dx,dy)
A.stat <- A$Stat
A.trans <- A$Trans

# Load data for the 23 Oct (B.):
my.data <- LoadPointCloud(Trans=A.trans)
B <- Data.prepare(my.data,xlim,ylim,dx,dy)
B.stat <- B$Stat
B.trans <- B$Trans

# Load data for the 11 Nov (C.):
my.data <- LoadPointCloud(Trans=A.trans)
C <- Data.prepare(my.data,xlim,ylim,dx,dy)
C.stat <- C$Stat
C.trans <- C$Trans

# Load data for the 20 Dec (D.):
my.data <- LoadPointCloud(Trans=A.trans)
D <- Data.prepare(my.data,xlim,ylim,dx,dy)
D.stat <- D$Stat
D.trans <- D$Trans

# Load data for the 11 Jan (E.):
my.data <- LoadPointCloud(Trans=A.trans)
E <- Data.prepare(my.data,xlim,ylim,dx,dy)
E.stat <- E$Stat
E.trans <- E$Trans

# Load data for the 15 Feb (FF.):
my.data <- LoadPointCloud(Trans=A.trans)
FF <- Data.prepare(my.data,xlim,ylim,dx,dy)
FF.stat <- FF$Stat
EF.trans <- FF$Trans

# Load data for the 8 Mar (G.):
my.data <- LoadPointCloud(Trans=A.trans)
G <- Data.prepare(my.data,xlim,ylim,dx,dy)
G.stat <- G$Stat
G.trans <- G$Trans

# Load data for the 11 Jan (H.):
my.data <- LoadPointCloud(Trans=A.trans)
H <- Data.prepare(my.data,xlim,ylim,dx,dy)
H.stat <- H$Stat
H.trans <- H$Trans

#====
# Plotting

H.points <- H.stat$Density*dx*dy
A.points <- A.stat$Density*dx*dy
Snow.depth <- H.stat$Min-A.stat$Min
Snow.depth[H.points<10 | A.points<10] <- NA
image(Snow.depth,zlim=c(0,1))
contour(Snow.depth,add=T)

plot(Snow.depth,A.points)


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

require(fields)
my.zlim <- c(-.5,.5)


image.plot(AB.max,nlevel=64,zlim=my.zlim,main='23 Sept - 23 Oct')
image.plot(BC.max,nlevel=64,zlim=my.zlim,main='23 Oct - 11 Nov')
image.plot(CD.max,nlevel=64,zlim=my.zlim,main='11 Nov - 20 Dec')
image.plot(DE.max,nlevel=64,zlim=my.zlim,main='20 Dec - 11 Jan')
image.plot(EF.max,nlevel=64,zlim=my.zlim,main='11 Dec - 15 Feb')
image.plot(FG.max,nlevel=64,zlim=my.zlim,main='15 Feb - 8 Mar')
image.plot(GH.max,nlevel=64,zlim=my.zlim,main='8 Mar - 26 Mar')
image.plot(A.stat$Max-A.stat$Min,zlim=c(0,4),main='Veg thickness')
dev.off()

#=========


plot(G.stat$Min.pt[,1],G.stat$Min.pt[,3])











#=====
# TEST ZONE:
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









