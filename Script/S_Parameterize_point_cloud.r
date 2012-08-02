
March_8 <- LoadPointCloud()

my.data <- March_8$Data

summary(my.data)
# Initial parameters
dx <- 0.1
dy <- .1
Xlim <- c(0,5)
Ylim <- c(0,5)

my.data <- Truncate_XY(my.data,Xlim,Ylim)

my.stat <- Basic.statmap.ptcloud(my.data,dx,dy,Xlim,Ylim)






my.coords <- my.stat$coords
my.min <- my.stat$min
my.max <- my.stat$max
my.quantile <- my.stat$quantile
my.density <- my.stat$density
my.95 <- my.quantile[,,3]-my.quantile[,,1]
my.50 <- my.quantile[,,2]

my.pairs <- data.frame(density=as.vector(my.density),quantile.95=as.vector(my.95),quantile.50=as.vector(my.50),thick=as.vector(my.max-my.min))

pairs(~density+quantile.95+thick,data=my.pairs)



layout(t(c(1,1,1,1,1,2,2)))
image(my.coords[1,],my.coords[2,],log(my.density),xlab="x (m)", ylab="y (m)",col=heat.colors(30) ,main = "Point density of raw data in log(pts/dm2)" )
contour(my.density,add=TRUE,labcex=1.2)
legend.col(col=heat.colors(30),lev=my.data[,3],title="")
#   hist(density$z,breaks=seq(from=0,to=max(density$z)+100, by= 50))
hist(log(my.density), main = "Histogram of point density", xlab="Point density")


layout(t(c(1,1,1,1,1,2,2)))
image(my.coords[1,],my.coords[2,],(my.max-my.min),xlab="x (m)", ylab="y (m)",col=heat.colors(30) ,main = "Thickness of point cloud" )
contour((my.max-my.min),add=TRUE,labcex=1.2)
legend.col(col=heat.colors(30),lev=my.data[,3],title="")
#   hist(density$z,breaks=seq(from=0,to=max(density$z)+100, by= 50))
hist((my.max-my.min), main = "Histogram of point cloud thickness", xlab="Height (m)")


layout(t(c(1,1,1,1,1,2,2)))
image(my.coords[1,],my.coords[2,],log(my.95),xlab="x (m)", ylab="y (m)",col=heat.colors(30) ,main = "95% of point cloud" )
contour(my.95,add=TRUE,labcex=1.2)
legend.col(col=heat.colors(30),lev=my.data[,3],title="")
#   hist(density$z,breaks=seq(from=0,to=max(density$z)+100, by= 50))
hist(my.95, main = "Histogram of point cloud thickness", xlab="Height (m)")


layout(t(c(1,1,1,1,1,2,2)))
image(my.coords[1,],my.coords[2,],(my.50),xlab="x (m)", ylab="y (m)",col=heat.colors(30) ,main = "95% of point cloud" )
contour(my.50,add=TRUE,labcex=1.2)
legend.col(col=heat.colors(30),lev=my.data[,3],title="")
#   hist(density$z,breaks=seq(from=0,to=max(density$z)+100, by= 50))
hist(my.50, main = "Histogram of point cloud thickness", xlab="Height (m)")


png("pairs.png",640,480)
pairs(my.pairs)
dev.off()






