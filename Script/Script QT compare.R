# Simon Filhol, 3rd May 2012
# code to compare AGL ground estimate from QTM to results from Kriging or other spatial modelling.

# based on file: grd_QT.txt

dxdy <- 0.100000001490
xmin <- -0.103000000119
ymin <- -12.115600395948

xmax <- (dim(grd_QT)[2]-1)*dxdy+xmin
  ymax <- (dim(grd_QT)[1]-1)*dxdy+ymin

xqt <- seq(from=xmin, by=dxdy, to= (dim(grd_QT)[2]-1)*dxdy+xmin)
yqt <- seq(from=ymin, by=dxdy, to= (dim(grd_QT)[1]-1)*dxdy+ymin)

my.list <- matrix(NA,nrow=length(xqt)*length(yqt),ncol=3)
k <- 1
for(i in 1:length(xqt)){
  for(j in 1:length(yqt)){
    my.list[k,3] <- grd_QT[j,i]
  k <- k+1  
  }
  
  
}

my.list[,1] <- xqt
my.list[,2] <- yqt

dat <- Truncate_XY(my.list,Xlim=c(0,2),Ylim=c(-12,-10))

my.dat <- matrix(NA,ncol=sqrt(length(dat[,1])),nrow=sqrt(length(dat[,1])))
k <- 1
for(i in 1:sqrt(length(dat[,1]))){
  for(j in 1:sqrt(length(dat[,1]))){
    my.dat[j,i] <- dat[k,3]
k <- k+1
    }
}

image(x=seq(from=data[1,1],to=max(data[,1]),by=dxdy),y=seq(from=data[1,2],to=max(data[,2]),by=dxdy),z=my.dat)
contour(my.dat,add=TRUE)

layout(1,2,3,n=3)
hist(my.dat,probability=TRUE, main="QTM")
hist(my.kr.results$predic,probability=TRUE, main="krig")
hist(my.interp$z,probability=TRUE, main="linear interp")
boxplot(my.kr.results$predic,my.dat,my.interp$z)
boxplot(dat[,3],smin[,3])
