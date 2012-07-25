DGPS_grd_truth <- read.delim("F:/Phd/Course/Spatial Stat/Project/final_product/DGPS_grd_truth.txt", header=F)
DGPS <- cbind(DGPS_grd_truth[,2],DGPS_grd_truth[,1],DGPS_grd_truth[,3])
DGPS <- Truncate_XY(DGPS,Xlim+East.offset,Ylim+North.offset)


my.east <- my.grid[,1]+East.offset 
my.north <- my.grid[,2]+North.offset

kriged <- cbind(my.east,my.north,my.kr.pred)
rm(my.east,my.north)


ind <- rep(NA,times=length(DGPS[,1]))
for(k in 1:length(DGPS[,1])){
ind[k] <- which(kriged[,1]>DGPS[k,1]-0.005 & kriged[,1]<DGPS[k,1]+0.005 & kriged[,2]>DGPS[k,2]-0.005 & kriged[,2]<DGPS[k,2]+0.005)
}

plot(kriged[ind,3],DGPS[,3])


library(gplots)

high <- my.kr.pred[ind]+1.96*my.kr.sd[ind]
low <- my.kr.pred[ind]-1.96*my.kr.sd[ind]
plotCI(DGPS[,3],kriged[ind,3],ui=high,li=low, xlab="DGPS elevation (m)",ylab="Kriging derived Elevation (m)")
lines(c(255,256.5),c(255,256.5))