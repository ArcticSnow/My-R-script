# 13 November 2012, Simon Filhol,
# 
# Script to derive snow depth map from LiDAR winter 2011/2011 at Glenn Creek
# 
# MAKE SURE TO LOAD FUNCTIONs:
#     - LoadPointCloud.r
#     - Basic_statmap_ptcloud.r
#     - Elementary functions.r
#     - FitPlane.r

setwd("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Winter 11_12/Lidar Survey/UTM all/subselection_2")


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
rm(A)

A.dim <- dim(A.stat$Min)
A <- data.frame(
  x=rep(A.stat$X,each=length(A.stat$X)),
  y=rep(A.stat$Y,A.dim[2]/length(A.stat$Y)),
  Min=as.vector(A.stat$Min))
  



# Load data for the 23 Oct (B.):
my.data <- LoadPointCloud(Trans=A.trans)
B <- Data.prepare(my.data,xlim,ylim,dx,dy)
B.stat <- B$Stat
B.trans <- B$Trans
rm(B)

B.dim <- dim(B.stat$Min)
B <- data.frame(
  x=rep(B.stat$X,each=length(B.stat$X)),
  y=rep(B.stat$Y,B.dim[2]/length(B.stat$Y)),
  Min=as.vector(B.stat$Min))


# Load data for the 11 Nov (C.):
my.data <- LoadPointCloud(Trans=A.trans)
C <- Data.prepare(my.data,xlim,ylim,dx,dy)
C.stat <- C$Stat
C.trans <- C$Trans
rm(C)


# Load data for the 20 Dec (D.):
my.data <- LoadPointCloud(Trans=A.trans)
D <- Data.prepare(my.data,xlim,ylim,dx,dy)
D.stat <- D$Stat
D.trans <- D$Trans
rm(D)


# Load data for the 11 Jan (E.):
my.data <- LoadPointCloud(Trans=A.trans)
E <- Data.prepare(my.data,xlim,ylim,dx,dy)
E.stat <- E$Stat
E.trans <- E$Trans
rm(E)


# Load data for the 15 Feb (FF.):
my.data <- LoadPointCloud(Trans=A.trans)
FF <- Data.prepare(my.data,xlim,ylim,dx,dy)
FF.stat <- FF$Stat
EF.trans <- FF$Trans
rm(FF)


# Load data for the 8 Mar (G.):
my.data <- LoadPointCloud(Trans=A.trans)
G <- Data.prepare(my.data,xlim,ylim,dx,dy)
G.stat <- G$Stat
G.trans <- G$Trans
rm(G)

# Load data for the 11 Jan (H.):
my.data <- LoadPointCloud(Trans=A.trans)
H <- Data.prepare(my.data,xlim,ylim,dx,dy)
H.stat <- H$Stat
H.trans <- H$Trans
rm(H)

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

#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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









