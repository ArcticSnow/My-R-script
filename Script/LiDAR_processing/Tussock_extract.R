#========================================================
# Tussock amplitude and wave length, Imnavait creek
#========================================================
# by Simon Filhol, 16 November 2012
# REQUIRES data from Lacie hardrive.
# REQUIRES function:
#     - LoadPointCloud.r
#     - Basic_statmap_ptcloud.r
#     - Elementary functions.r
#     - FitPlane.r

setwd("/Volumes/LaCie/Snowstar2012/GrdLiDAR/Product")
require(raster)
require(fields)
require(pracma)
require(rgdal)

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
xlim <-c(3,22)
ylim <- c(4,24)
dx <- 0.05
dy <- 0.05

# Load data for the 23 Sept (A.):
my.data <- LoadPointCloud()
A <- Data.prepare(my.data,xlim,ylim,dx,dy)
#rm(my.data)
A.stat <- A$Stat
A.trans <- A$Trans

A.dim <- dim(A.stat$Min)
A.df <- data.frame(
  x=rep(A.stat$X,length(A.stat$Y)),
  y=rep(A.stat$Y,each=length(A.stat$Y),length.out=(length(A.stat$Y)*length(A.stat$X))),
  Min=as.vector(A.stat$Min))

Bs <- FitPlane(cbind(A.df$x[!is.na(A.df$Min)],A.df$y[!is.na(A.df$Min)],A.df$Min[!is.na(A.df$Min)]))

plane <- (-(A.df$x*Bs[1]+A.df$y*Bs[2]+Bs[4])/Bs[3])
plane <- Reshape(plane,length(A.stat$X),length(A.stat$Y))
ground <- A.stat$Min-plane

plot(A.df$x,A.df$Min)

image.plot(ground,nlevel=64)
contour((ground),add=T)
# 
# require(akima)
# interp.NA <- interp(A.stat$Min.pt[,1],A.stat$Min.pt[,2],A.stat$Min.pt[,3],
#                     xo=seq(from=xlim[1],to=xlim[2]-dx,by=dx),
#                     yo=seq(from=ylim[1],to=ylim[2]-dy,by=dy))
# image.plot(interp.NA)
# 
# a <- image.smooth(ground)
# image.plot(a)


# Create a raster object and save as a geotiff
my.raster <- raster(t(ground), 
                    xmn=min(A.stat$X),
                    xmx=max(A.stat$X),
                    ymn=min(A.stat$Y),
                    ymx=max(A.stat$Y)
                    )
image(my.raster)

writeRaster(my.raster,filename="Tussock_imnavait.tif",format="GTiff",overwrite=TRUE)
write.table(ground,file="Tussock_imnavait.csv", sep=",", col.names=F,row.names=F)


