# By Simon Filhol, April 2nd, 2013
# 
# Detrending function
# Input Parameters are: 
#    - Gridded.XYZ  <-  an XYZ matrix of the data to be detrended
#    - Map.output <- FALSE or TRUE. TRUE if you want a map as output. 
#    - Map.size <- a 2x1 vector indicateing the size of the map. Length(Gridded.XYZ) = Map.size[1]*Map.size[2]

Detrend <- function(Gridded.XYZ,Map.output,Map.size){
  # Load elementary funcions for point cloud
  ifelse({Sys.info()['sysname']=="Windows"},{
    source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/FitPlane.r")
  },{
    source("/Users/simonfilhol/github/local/My_R_script/Function/FitPlane.R")
  })

  Bs <- FitPlane(cbind(Gridded.XYZ[!is.na(Gridded.XYZ[,3]),1],Gridded.XYZ[!is.na(Gridded.XYZ[,3]),2],Gridded.XYZ[!is.na(Gridded.XYZ[,3]),3]))
  ground.xyz <- -(Gridded.XYZ[,1]*Bs[1]+Gridded.XYZ[,2]*Bs[2]+Bs[4]+Gridded.XYZ[,3]*Bs[3])
  
  if(Map.output==TRUE){
  library(pracma)
  if(Map.size[1]*Map.size[2]!=length(Gridded.XYZ[,1])){stop("Detrend Function error: Input data do not correspond to the size of map indicated")}
  ground <- Reshape(ground.xyz,Map.size[1],Map.size[2])
  }else{ground <- ground.xyz}
  return(ground)
}


# Function to convert a 2D matrix in a XYZ columns matrix.
# Three possible way to go: 
#    - provide (Map.Z, Map.X, Map.Y)
#    - provide (Map.Z, dx, dy, Xlim, Ylim)
#    - provide (Map.Z, X.vec, Y.vec)

Map2xyz <- function(Map.Z,Map.X,Map.Y,dx,dy,Xlim,Ylim,X.vec,Y.vec){
  if(!missing(Map.X)& !missing(Map.Y)){
    my.xyz <- cbind(as.vector(Map.X),as.vector(Map.Y),as.vector(Map.Z))
  }
  if(!missing(dx) & !missing(dy) & !missing(Xlim) & !missing(Ylim)){
    
    my.xyz <- cbind(rep(X.vec,each=length(X.vec)),
                    rep(Y.vec,A.dim[2]/length(Y.vec)),
                    as.vector(Map.Z))
    
  }
  if(!missing(X.vec)& !missing(Y.vec)){
    my.xyz <- cbind(rep(X.vec,each=length(X.vec)),
                    rep(Y.vec,A.dim[2]/length(Y.vec)),
                    as.vector(Map.Z))
  }
  return(my.xyz)
}


# Example
# ifelse({Sys.info()['sysname']=="Windows"},{
#   load("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Winter 11_12/Lidar Survey/UTM all/subselection_2/Clean/R_workspace.RData")
# },{
#   load("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Winter 11_12/Lidar Survey/UTM all/subselection_2/Clean/R_workspace.RData")
# })
# test <- Detrend(Gridded.XYZ=Map2xyz(A.stat$Min,X.vec=A.stat$X,Y.vec=A.stat$Y),Map.output=TRUE,Map.size=c(250,250))
