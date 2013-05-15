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

Ptc2Surfmat <- function(data,Xlim,Ylim,dx,dy,Detrended,Detrend.matrix, Matname, DataInR){
  
  # Required packages
  require(R.matlab)
  require(akima)
  
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
  
  
  # something wrong with xo and yo. 
  my.data.stat$Min.interp<- interp(my.data.stat$Min.pt[,1],my.data.stat$Min.pt[,2],my.data.stat$Min.pt[,3],xo=my.data.stat$Xcoord,yo=my.data.stat$Ycoord,duplicate='mean')$z
  my.data.stat$Max.interp<- interp(my.data.stat$Max.pt[,1],my.data.stat$Max.pt[,2],my.data.stat$Max.pt[,3],xo=my.data.stat$Xcoord,yo=my.data.stat$Ycoord,duplicate='mean')$z
  # need to use inter2xyz() instead of Map2xyz() in Detrended section
    image(my.data.stat$Min.interp)
  
  if(Detrended==FALSE){
    Map.length <- (Xlim[2]-Xlim[1])/dx
    Map.height <- (Ylim[2]-Ylim[1])/dy
    my.min.xyz <- Map2xyz(Map.Z=my.data.stat$Min.interp,dx=my.dx,dy=my.dy,Xlim=Xlim,Ylim=Ylim)
    print(my.min.xyz[1:5,])
    my.ground <- Detrend(Gridded.XYZ=my.min.xyz,Map.output=TRUE,Map.size=c(Map.length,Map.height))
    Detrend.matrix <- my.ground-my.data.stat$Min.interp
  }
  
  my.surface.min <- my.data.stat$Min.interp+Detrend.matrix
  my.surface.max <- my.data.stat$Max.interp+Detrend.matrix
  
  # Save my.surface into a Matlab m-file ====
  Detrended <- TRUE   # are the data detrended? I/O
  writeMat(Matname,
           my_surface_min=my.surface.min,
           my_surface_max=my.surface.max,
           # add any other surface of interest
           dx=dx,dy=dy,Detrended=Detrended)
  
  # Put data in list for return
  if(DataInR==TRUE){
    Data.to.return <- list(my.data=my.data,
                           my_surface_min=my.surface.min,
                           my_surface_max=my.surface.max,
                           # add any other surface of interest
                           my.detrend.matrix=ground)
    return(Data.to.return)
  }
}