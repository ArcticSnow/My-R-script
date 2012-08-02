# Simon Filhol, 1st august 2012
#

Basic.statmap.ptcloud <- function(data,dx,dy,Xlim,Ylim){

# Initialize 
x.res <- seq(from=Xlim[1],to=Xlim[2]-dx,by=dx)
y.res <- seq(from=Ylim[1],to=Ylim[2]-dy,by=dy)
z.density <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
z.min <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
z.max <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
z.quantile <- array(NA,dim=c(length(x.res),length(y.res),3))

# Loop to split data in pixels dx*dy and extract statistique parameters
k <- 1
for(i in x.res){
  m <- 1
  for(j in y.res){
    ind <- which((data[,1]>=i)&(data[,1]<(i+dx))&(data[,2]>=j)&(data[,2]<(j+dy)))
    z.density[k,m] <- length(ind)/(dx*dy)
    z.min[k,m] <- min(data[ind,3])
    z.max[k,m] <- max(data[ind,3])
    z.quantile[k,m,] <- quantile(data[ind,3],probs=c(0.025,0.5,0.975),na.rm=TRUE)
    m <- m+1
  } 
  k <- k+1
}

All <- list(coords=rbind(x.res+dx/2,y.res+dy/2),density=z.density,min=z.min,max=z.max,quantiles=z.quantile)

return(All)
}