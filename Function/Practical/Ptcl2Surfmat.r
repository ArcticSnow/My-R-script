# Simon Filhol, 28 March 2013
# From raw point cloud to a surfaces in ".mat" format

# function inputs
#  - Xlim  <-  Limit of the window of interest in the X-directioin in 
#              the same unit as the input data  ex: Xlim=c(0,10). Has to bee multiple of dx
#  - Ylim  <-  Limit of the window of interest in the Y-directioin in 
#              the same unit as the input data  ex: Ylim=c(0,10). Has to be multiple of dy
#  - dx  <-    Size of the final map pixel in x-direction. Same unit as input data
#  - dy  <-    Size of the final map pixel in y-direction. Same unit as input data
#  - detrend <- 
#  - file.path  <-  file path containing the Point cloud data. Has to be a 
#              3 columns XYZ text file
#  - Trans  <- Specify if needs to apply a specific translation to the data
#  - Matname  <-  Name of the output .mat file. Will be saved in the priject 
#                 directory. To change, use setwd() function. The output file will be a 2D matrix
#  - DataInR <- TRUE or FALSE if need to return data in R

Ptc2Surfmat <- function(data,Xlim,Ylim,dx,dy,Detrended,Detrend.matrix, Matname, DataInR, XYZorTRI){
  
  # Required packages
  require(R.matlab)
  
  # Load elementary funcions for point cloud
  R.git.dir <- function(){
    if(Sys.info()['nodename']=="Simon-Filhols-MacBook-Pro.local")
    {
      my.R.git.dir <- '/Users/simonfilhol/github/local/My_R_script'
    }else{
      if(Sys.info()['nodename']=='LIDAR-PC'){
        my.R.git.dir <- 'C:/Users/LiDAR/GitHub/My-R-script'
      }else{
        my.R.git.dir <- "C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script"
      }
    }
  }
source(paste(R.git.dir(),"Function/Practical/Elementary_functions.r",sep="/"))
source(paste(R.git.dir(),"Function/Practical/Detrend.r",sep="/"))

  # Check input, and assign default value if variable not specified as function input 
  if(missing(DataInR)){DataInR=FALSE}
  if(missing(dx)){dx <- (Xlim[2]-Xlim[1])/250}
  if(missing(dy)){dy <- (Ylim[2]-Ylim[1])/250}
  if(missing(XYZorTRI)){XYZorTRI <- "XYZ"}
  if(Detrended==TRUE & missing(Detrend.matrix)){stop('Cannot have Detrended and Detrend.matrix at same time')}
  my.dx <- dx
  my.dy <- dy
  my.data <- Data.prepare(my.xyz=data,
                          translation=c(0,0,0),
                          xlim=Xlim,
                          ylim=Ylim,
                          dx=my.dx,
                          dy=my.dy,
                          return.xyz=TRUE)
  my.data.stat <- my.data$Stat
  my.data.trans <- my.data$Trans
  rm(my.data)
  
  if(XYZorTRI=="XYZ"){
    require(akima)
    Min.interp<- interp(my.data.stat$Min.pt[,1],my.data.stat$Min.pt[,2],my.data.stat$Min.pt[,3],xo=seq(Xlim[1]+dx/2,Xlim[2],dx),yo=seq(Ylim[1]+dy/2,Ylim[2],dy),duplicate='mean')
    Max.interp<- interp(my.data.stat$Max.pt[,1],my.data.stat$Max.pt[,2],my.data.stat$Max.pt[,3],xo=seq(Xlim[1]+dx/2,Xlim[2],dx),yo=seq(Ylim[1]+dy/2,Ylim[2],dy),duplicate='mean')
  }
  else{
    require(geometry)
    Min.interp <- delaunayn(my.data.stat)
  }
  if(Detrended==FALSE){
    Map.length <- (Xlim[2]-Xlim[1])/dx
    Map.height <- (Ylim[2]-Ylim[1])/dy
    my.min.xyz <- interp2xyz(Min.interp,data.frame=TRUE)
    print(my.min.xyz[1:10,])
    my.ground <- Detrend(Gridded.XYZ=my.min.xyz,Map.output=TRUE,Map.size=c(Map.length,Map.height))
    Detrend.matrix <- my.ground-Min.interp$z
  }
  my.surface.min <- Min.interp$z+Detrend.matrix
  my.surface.max <- Max.interp$z+Detrend.matrix
  
  # Save my.surface into a Matlab m-file ====
  Detrended <- TRUE   # are the data detrended? I/O
  writeMat(Matname,
           my_surface_min=my.surface.min,
           my_surface_max=my.surface.max,
           # add any other surface of interest
           dx=dx,dy=dy,Detrended=Detrended)
  
  # Put data in list for return
  if(DataInR==TRUE){
    Data.to.return <- list(my.data.stat=my.data.stat,
                           my.data.trans=my.data.trans,
                           my_surface_min=my.surface.min,
                           my_surface_max=my.surface.max,
                           # add any other surface of interest
                           my.detrend.matrix=my.ground)
    return(Data.to.return)
  }
}