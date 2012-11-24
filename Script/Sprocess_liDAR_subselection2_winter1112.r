# 13 November 2012, Simon Filhol,
# 
# Script to derive snow depth map from LiDAR winter 2011/2011 at Glenn Creek
# 
# MAKE SURE TO LOAD FUNCTIONs:
#     - LoadPointCloud.r
#     - Basic_statmap_ptcloud.r
#     - Elementary functions.r
#     - FitPlane.r

setwd("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Winter 11_12/Lidar Survey/UTM all/subselection_2")
LoadPointCloud <- function(Trans){
  
  library("tcltk")
  
  # Test the type of OS, windows or mac
  if(Sys.info()['sysname']=="Windows"){
    my.dir.orig <- tk_choose.dir(default = "C:/Documents and Settings", caption = "Select the file directory")
  }
  if(Sys.info()['sysname']=="Darwin"){
    my.dir.orig <- tk_choose.dir(default = "/Users", caption = "Select the file directory")
  }
  if(is.na(my.dir.orig)){break}
  my.file.name <- dir(my.dir.orig)
  
  
  repeat {
    print("Which file to use? ")
    print(my.file.name)
    ind <- as.numeric(readline("Give row number for file to open: "))
    my.file <- my.file.name[ind]
    print(my.file)
    OK <- as.numeric(readline("Is it the right file (0/1)? "))
    if(OK>0){break}
  }
  
  
  # Load data into 3 column matrix
  raw.data <- read.table( paste(my.dir.orig,"/",my.file,sep=""), quote="\"")
  my.data <- matrix(c(raw.data[,1],raw.data[,2],raw.data[,3]),ncol=3)
  rm(raw.data)
  my.data <- round(my.data,3)
  
  # Translate data to reduce memory use
  
  if(my.data[1,1]>500){
    if(hasArg(Trans)){
      my.trans.xyz <- Trans
    }
    else{
      my.trans.xyz <- my.data[1,]
    }
    print(paste("Translation apply =",my.trans.xyz))
    my.data <- cbind(my.data[,1]-my.trans.xyz[1], my.data[,2]-my.trans.xyz[2],my.data[,3]-my.trans.xyz[3])
    my.info <- list(Directory=my.dir.orig,Filelist=my.file.name,Translation=my.trans.xyz,Data= my.data)
  }
  else{
    my.info <- list(Directory=my.dir.orig,Filelist=my.file.name,Data= my.data) 
  }
  return(my.info)
}
Basic.statmap.ptcloud <- function(data,dx,dy,Xlim,Ylim){
  
  if(Xlim[1]<min(data[,1]) | Xlim[2]>max(data[,1])  | Ylim[1]<min(data[,2])  | Ylim[2]>max(data[,2]) ){
    stop("Limits are wider than actual data")
  }
  
  
  data <- data[data[,1]>=Xlim[1] & data[,1] <=Xlim[2],]
  data <- data[data[,2]>=Ylim[1] & data[,2] <=Ylim[2],]
  
  
  # Initialize 
  x.res <- seq(from=Xlim[1],to=Xlim[2]-dx,by=dx)
  y.res <- seq(from=Ylim[1],to=Ylim[2]-dy,by=dy)
  z.density <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
  z.min <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
  z.max <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
  z.mean <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
  z.std <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
  z.quantile <- array(NA,dim=c(length(x.res),length(y.res),3))
  z.pt.loc.min <- matrix(NA,nrow=length(x.res)*length(y.res),ncol=3 )
  
  # Loop to split data in pixels dx*dy and extract statistique parameters
  time <- Sys.time()
  
  k <- 1
  for(i in x.res){
    m <- 1
    ind1 <- ((data[,1]>=i)&data[,1]<(i+dx))
    
    ifelse(length(ind1[ind1==T]>=1),{
      inter1 <- data[ind1,]
      for(j in y.res){
        ind2 <- (inter1[,2]>=j)&(inter1[,2]<(j+dy))
        ifelse(length(ind2[ind2==T]>=1),{
          inter2 <- inter1[ind2,3]
          z.density[k,m] <- length(inter2)/(dx*dy)
          z.min[k,m] <- min(inter2,na.rm=T)
          z.max[k,m] <- max(inter2,na.rm=T)
          z.mean[k,m] <- mean(inter2,na.rm=T)
          z.std[k,m] <- sd(inter2,na.rm=T)
          z.quantile[k,m,] <- quantile(inter2,probs=c(0.025,0.5,0.975),na.rm=TRUE)
          inter3 <- inter1[inter1[ind2,3]==min(inter2,na.rm=T),]
          z.pt.loc.min[k+m,] <- ifelse(dim(inter3)[1]==1,inter3,inter3[1,])
        },{
          z.min[k,m] <- NA 
          z.density[k,m] <- NA
          z.min[k,m] <- NA
          z.max[k,m] <- NA
          z.mean[k,m] <- NA
          z.std[k,m] <- NA
          z.quantile[k,m,] <- c(NA,NA,NA) 
          z.pt.loc.min[k+m,] <-c(NA,NA,NA) 
        })
        m <- m+1
      }
      data <- data[!ind1, ]
    },{
      z.min[k,] <- NA 
      z.density[k,] <- NA
      z.min[k,] <- NA
      z.max[k,] <- NA
      z.mean[k,] <- NA
      z.std[k,] <- NA
      z.quantile[k,,] <- c(NA,NA,NA)
      z.pt.loc.min[k+m,] <-c(NA,NA,NA) 
    })
    
    k <- k+1
  }
  print(Sys.time()-time)
  
  z.pt.loc.min <- z.pt.loc.min[!is.na(z.pt.loc.min[,3]),]  
  
  All <- list(coords=rbind(x.res+dx/2,y.res+dy/2),
              Density=z.density,
              Min=z.min,
              Max=z.max,
              Mean=z.mean,
              Sd=z.std,
              Quantiles=z.quantile,
              Min.pt=z.pt.loc.min,
              X=x.res,
              Y=y.res
              
  )
  return(All)
}
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
xlim <-c(1,11)
ylim <- c(1,11)
dx <- 0.1
dy <- 0.1

# Load data for the 23 Sept (A.):
my.data <- LoadPointCloud()
A <- Data.prepare(my.data,xlim,ylim,dx,dy)
A.stat <- A$Stat
A.trans <- A$Trans
rm(A)

A.dim <- dim(A.stat$Min)
A <- data.frame(
  x=rep(A.stat$X,each=length(A.stat$X)),
  y=rep(A.stat$Y,A.dim[2]/length(A.stat$Y)),
  Min=as.vector(A.stat$Min))
  



# Load data for the 23 Oct (B.):
my.data <- LoadPointCloud(Trans=A.trans)
B <- Data.prepare(my.data,xlim,ylim,dx,dy)
B.stat <- B$Stat
B.trans <- B$Trans
rm(B)

B.dim <- dim(B.stat$Min)
B <- data.frame(
  x=rep(B.stat$X,each=length(B.stat$X)),
  y=rep(B.stat$Y,B.dim[2]/length(B.stat$Y)),
  Min=as.vector(B.stat$Min))


# Load data for the 11 Nov (C.):
my.data <- LoadPointCloud(Trans=A.trans)
C <- Data.prepare(my.data,xlim,ylim,dx,dy)
C.stat <- C$Stat
C.trans <- C$Trans
rm(C)


# Load data for the 20 Dec (D.):
my.data <- LoadPointCloud(Trans=A.trans)
D <- Data.prepare(my.data,xlim,ylim,dx,dy)
D.stat <- D$Stat
D.trans <- D$Trans
rm(D)


# Load data for the 11 Jan (E.):
my.data <- LoadPointCloud(Trans=A.trans)
E <- Data.prepare(my.data,xlim,ylim,dx,dy)
E.stat <- E$Stat
E.trans <- E$Trans
rm(E)


# Load data for the 15 Feb (FF.):
my.data <- LoadPointCloud(Trans=A.trans)
FF <- Data.prepare(my.data,xlim,ylim,dx,dy)
FF.stat <- FF$Stat
EF.trans <- FF$Trans
rm(FF)


# Load data for the 8 Mar (G.):
my.data <- LoadPointCloud(Trans=A.trans)
G <- Data.prepare(my.data,xlim,ylim,dx,dy)
G.stat <- G$Stat
G.trans <- G$Trans
rm(G)

# Load data for the 11 Jan (H.):
my.data <- LoadPointCloud(Trans=A.trans)
H <- Data.prepare(my.data,xlim,ylim,dx,dy)
H.stat <- H$Stat
H.trans <- H$Trans
rm(H)

# Plotting

H.points <- H.stat$Density*dx*dy
A.points <- A.stat$Density*dx*dy
Snow.depth <- H.stat$Min-A.stat$Min
Snow.depth[H.points<10 | A.points<10] <- NA
image(Snow.depth,zlim=c(0,1))
contour(Snow.depth,add=T)

plot(Snow.depth,A.points)


Bs <- FitPlane(cbind(A$x[!is.na(A$Min)],A$y[!is.na(A$Min)],A$Min[!is.na(A$Min)]))
library(pracma)
ground <- -(A$x*Bs[1]+A$y*Bs[2]+Bs[4]+A$Min*Bs[3])
ground <- Reshape(ground,length(A.stat$X),length(A.stat$Y))

plot(ground,Snow.depth)

#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# TEST ZONE:
# 
# setwd("/Users/simonfilhol/Desktop")
# library(R.matlab)
# writeMat("SnowDepth.mat",Depth=Snow.depth)
# writeMat("Ground_d.mat",Ground=ground)
# 


Canopy <- A.stat$Max-A.stat$Min
#Canopy[Canopy>1.5] <- NA
image(Canopy)
contour(Canopy,add=T)
hist(Canopy,100)









