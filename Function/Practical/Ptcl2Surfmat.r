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

Ptc2Surfmat <- function(file.path,Xlim,Ylim,dx,dy,Detrended,Detrend.matrix,Trans, Matname, DataInR){
  
  # Required packages
  require(R.matlab)
  require(akima)
  
  # Load elementary funcions for point cloud
  ifelse({Sys.info()['sysname']=="Windows"},{
    source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/Elementary functions.r")
    source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/Detrend.r")
  },{
    source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/Elementary_functions.r")
    source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/Detrend.r")
  })
  
  # PROBLEM WITH TRANS and cannnot call my.dat vefore it got loaded. REVIEW this frist section
  
  # Check input, and assign default value if variable not specified as function input
  ifelse(missing(file.path),{my.file <- file.choose()},{my.file <- file.path})
  if(hasArg(Trans)){my.trans.xyz <- Trans}
  else{my.trans.xyz <- my.data[1,]}
  if(missing(DataInR)){DataInR=FALSE}
  if(missing(Xlim)){Xlim <- c(min(my.data$Data[,1],na.rm=TRUE),max(my.data$Data[,1],na.rm=TRUE))}
  if(missing(Ylim)){Ylim <- c(min(my.data$Data[,2],na.rm=TRUE),max(my.data$Data[,2],na.rm=TRUE))}
  if(missing(dx)){dx <- (Xlim[2]-Xlim[1])/250}
  if(missing(dy)){dy <- (Ylim[2]-Ylim[1])/250}
  if(Detrended==TRUE & missing(Detrend.matrix)){stop('Cannot have Detrended and Detrend.matrix at same time')}
  
  # Load data
  data <- LoadPointCloud(Trans=Trans,file=my.file)
  my.data <- Data.prepare(data,xlim,ylim,dx,dy,return.xyz=TRUE)
  my.data.stat <- my.data$Stat
  my.data.trans <- my.data$Trans
    my.data.stat$Min.interp<- interp(my.data.stat$Min.pt[,1],my.data.stat$Min.pt[,2],my.data.stat$Min.pt[,3],xo=my.data.stat$X,yo=my.data.stat$Y,duplicate='mean')$z
  my.data.stat$Max.interp<- interp(my.data.stat$Max.pt[,1],my.data.stat$Max.pt[,2],my.data.stat$Max.pt[,3],xo=my.data.stat$X,yo=my.data.stat$Y,duplicate='mean')$z
  
  if(Detrended==False){
    Map.length <- (Xlim[2]-Xlim[1])/dx
    Map.height <- (Ylim[2]-Ylim[1])/dy
    my.min.xyz <- Map2xyz(Map.Z=my.data.stat$Min.interp,X.vec=my.data.stat$X,Y.vec=my.data.stat$Y)
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