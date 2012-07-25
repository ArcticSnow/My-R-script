
flat.pts.simul <- function(scan.h,dxdy,res.dist,xmax){
# 
# scan.h <- 2
# dxdy <- .01
# res.dist <- 100
# 
# xmax <- 100
# 
# 
# 
# #convert angle
# slope <- slope*pi/180

my.theta <- seq(from=-45*pi/180, to=pi/2, by=2*atan((dxdy/2)/res.dist))
my.phi <- seq(from=0, to=2*pi,by=2*atan((dxdy/2)/res.dist))

ind <- which(my.theta<slope)

xi <- -scan.h*tan(pi/2-my.theta[ind])
med <- median(xi)
xi <- xi[which(xi<xmax)]
# hist(xi[ind],xlim=c(0,xmax),breaks=100)
# plot(ecdf(xi))

results <- c(med,length(xi))
return(results)
}


#########################################################################
# Script to evaluate diagrams




my.dxdy <- seq(from=.01, to=.06, by=0.0001)
res.dist <- seq(from=5,to=45,by=.2)
scan.h <- seq(from=1,to=8,by=.1)


#
my.output <- matrix(NA,ncol=2,nrow=length(my.dxdy)*length(res.dist))
my.medians <- matrix(NA,ncol=length(res.dist),nrow=length(my.dxdy))
my.total.pts <- my.medians

#study influence of my.dxdy on total number of points and median distances of 50% points included
k <- 1
for(i in 1:length(my.dxdy)){
  for(j in 1:length(res.dist)){
    my.output[k,] <- flat.pts.simul(2,my.dxdy[i],res.dist[j],100)
    my.medians[i,j] <- my.output[k,1]
    my.total.pts[i,j] <- my.output[k,2]
    
    k <- k+1
  }
}
image(my.dxdy,res.dist,my.medians)
# contour(my.dxdy,res.dist,my.medians,add=TRUE,cexlab=1.2)

image(my.dxdy,res.dist,my.total.pts)
contour(my.dxdy,res.dist,my.total.pts,add=TRUE,cex=1.5)


# study of height of lidar
my.output <- matrix(NA,ncol=2,nrow=length(res.dist)*length(scan.h))
my.medians <- matrix(NA,ncol=length(scan.h),nrow=length(res.dist))
my.total.pts <- my.medians
k <- 1
for(i in 1:length(res.dist)){
  for(j in 1:length(scan.h)){
    my.output[k,] <- flat.pts.simul(scan.h[j],0.1,res.dist[i],100)
    my.medians[i,j] <- my.output[k,1]
    my.total.pts[i,j] <- my.output[k,2]
    
    k <- k+1
  }
}
plot(scan.h,my.medians[1,],type="l")

image(res.dist,scan.h,my.total.pts)
contour(res.dist,scan.h,my.total.pts,add=TRUE,cex=1.5)


# 
# x <- rep(NA,times=length(xi)*length(my.phi))
# y <- x
# 
# k <- 1
# for(i in 1:length(xi)){
#   for(j in 1:length(my.phi)){
# x[k] <- xi[i]*cos(my.phi[j])
# y[k] <- xi[i]*sin(my.phi[j])
# k <- k+1
# }
# }
# plot(x,y)





# fov.hor <- seq(from=0, to=2*pi, by=2*atan((dx/2)/res.dist))







