# Script that compare DGPS data to Raw and Kriged elevation data.
# Simon Filhol

# Problem to solve !!!!!!

ifelse({Sys.info()['sysname']=="Windows"},print('Good to go !!'),{stop('Needs to be on my office windows machine')})

setwd("F:/Phd/Course/Spatial Stat/Project")
# Load Data and functions
source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/ElemFunction/R/Elementary functions.r")
source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Legend colorscale function.r")
source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/Basic_statmap_ptcloud.r")


my.data <- LoadPointCloud()
print(paste('Xlim of my data = ',as.character(c(min(my.data$Data[,1]), max(my.data$Data[,1])))))
print(paste('Ylim of my data = ',as.character(c(min(my.data$Data[,2]), max(my.data$Data[,2])))))

# Initial parameters
Xlim <- c(0,6)
Ylim <- c(0,6)
dx <- .1
dy <- .1
my.krig.dxdy <- .01
North.offset <- my.data$Translation[2]
East.offset <- my.data$Translation[1]

my.maps <- Basic.statmap.ptcloud(my.data$Data,dx,dy,Xlim,Ylim)


# Load DGPS data of the Calib area from the 25 apr 2012
DGPS_grd_truth <- read.delim("F:/Phd/Course/Spatial Stat/Project/final_product/DGPS_grd_truth_25apr2012.txt", header=FALSE,skip=1)
DGPS <- cbind(DGPS_grd_truth[,2]-my.data$Translation[1],
              DGPS_grd_truth[,1]-my.data$Translation[2],
              DGPS_grd_truth[,3]-my.data$Translation[3])
DGPS <- Truncate_XY(DGPS,Xlim,Ylim)

x=rep(A.stat$X,each=length(A.stat$X)),
y=rep(A.stat$Y,A.dim[2]/length(A.stat$Y)),
Min=as.vector(A.stat$Min))

my.min <- cbind(rep((my.maps$X+dx/2),each=length(my.maps$X)),
                rep((my.maps$Y+dy/2),dim(my.maps$Min)[2]/length(my.maps$Y)),
                as.vector(my.maps$Min))

library(pracma)
my.min.reshaped <- Reshape(my.min[,3],length(my.maps$X),length(my.maps$Y))
image(my.min.reshaped-my.maps$Min)

Lidar.elev <- rep(NA,times=length(DGPS[,1]))

for(k in 1:length(DGPS[,1])){
  ind <- {my.min[,1]>=DGPS[k,1]-dx/2 & my.min[,1]<DGPS[k,1]+dx/2 & my.min[,2]>=DGPS[k,2]-dx/2 & my.min[,2]<DGPS[k,2]+dy/2}
  if(sum(ind)==1){Lidar.elev[k] <- my.min[ind,3]}
}


plot(DGPS[,1],DGPS[,2])
image(my.maps$Min)
plot(DGPS[,3]+my.data$Translation[3],Lidar.elev+my.data$Translation[3])










# 
# my.east <- my.grid[,1]+East.offset 
# my.north <- my.grid[,2]+North.offset
# 
# kriged <- cbind(my.east,my.north,my.kr.pred)
# rm(my.east,my.north)
# 
# 
# ind <- rep(NA,times=length(DGPS[,1]))
# for(k in 1:length(DGPS[,1])){
# ind[k] <- which(kriged[,1]>DGPS[k,1]-0.005 & kriged[,1]<DGPS[k,1]+0.005 & kriged[,2]>DGPS[k,2]-0.005 & kriged[,2]<DGPS[k,2]+0.005)
# }
# 
# plot(kriged[ind,3],DGPS[,3])
# 
# 
# library(gplots)
# 
# high <- my.kr.pred[ind]+1.96*my.kr.sd[ind]
# low <- my.kr.pred[ind]-1.96*my.kr.sd[ind]
# plotCI(DGPS[,3],kriged[ind,3],ui=high,li=low, xlab="DGPS elevation (m)",ylab="Kriging derived Elevation (m)")
# lines(c(255,256.5),c(255,256.5))