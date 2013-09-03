#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#          SCRIPT FOR SURFACE COMPARISON AND EVOLUTION
#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Simon Filhol, 23 January 2013


# MAKE SURE TO LOAD FUNCTIONs:
# Set path and load required functions ====

ifelse({Sys.info()['sysname']=="Windows"},{
  setwd("G:/GlennCreek/Grd_LiDAR/Glenn_Creek1112/UTM/subselection_2/Clean")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/FitPlane.r")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/Basic_statmap_ptcloud.r")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/LoadPointCloud.r")
  source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Practical/Multiplot.r")
  source("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Winter 11_12/Lidar Survey/UTM all/subselection_2/Clean/R_workspace.RData")
  },{
  setwd("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Winter 11_12/Lidar Survey/UTM all/subselection_2/Clean")
  source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/Basic_statmap_ptcloud.r")
  source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/LoadPointCloud.r")
  source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/Multiplot.R")
  source("/Users/simonfilhol/github/local/My_R_script/Function/FitPlane.R")
  load("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Winter 11_12/Lidar Survey/UTM all/subselection_2/Clean/R_workspace.RData")
})
require(fields)
require(abind)
require(R.matlab)
require(akima)

dev.off()

# TEST AREA FOR USABILITY OF AKIMA OVER IMAGE.SMOOTH ====
# try linear interpolation with akima and compare to image smooth. 
plot(B.stat$Max.pt[,1],B.stat$Max.pt[,3])

B.interp.min<- interp( B.stat$Min.pt[,1],B.stat$Min.pt[,2],B.stat$Min.pt[,3],xo=B.stat$X,yo=B.stat$Y,duplicate='mean')
image.plot(B.interp.min$z+(ground-A.stat$Min)-mean(B.interp.min$z+(ground-A.stat$Min),na.rm=TRUE))
B.stat$Max.pt <- B.stat$Max.pt[!is.na(B.stat$Max.pt[,3]),] 
B.interp.max<- interp( B.stat$Max.pt[,1],B.stat$Max.pt[,2],B.stat$Max.pt[,3],xo=B.stat$X,yo=B.stat$Y,duplicate='mean')
image.plot(B.interp.max$z+(ground-A.stat$Min)-mean(B.interp.max$z+(ground-A.stat$Min),na.rm=TRUE))


diff <- image.smooth(B.stat$Min+(ground-A.stat$Min)-mean(B.stat$Min+(ground-A.stat$Min),na.rm=TRUE))$z-(B.interp.min$z+(ground-A.stat$Min)-mean(B.interp.min$z+(ground-A.stat$Min),na.rm=TRUE))
image.plot(diff)
image.plot(image.smooth(B.stat$Min+(ground-A.stat$Min)-mean(B.stat$Min+(ground-A.stat$Min),na.rm=TRUE))$z)
image.plot(B.interp.min$z+(ground-A.stat$Min)-mean(B.interp.min$z+(ground-A.stat$Min),na.rm=TRUE))
#redo all surfaces with akima instead of image.smooth!!!!!!!!!!!


# PREPARATION of all surfaces (Min, Max, ground) with interp() of Akima package ====

# for Min map (linear interp)
A.stat$Min.interp<- interp(A.stat$Min.pt[,1],A.stat$Min.pt[,2],A.stat$Min.pt[,3],xo=A.stat$X,yo=A.stat$Y,duplicate='mean')$z
B.stat$Min.interp<- interp(B.stat$Min.pt[,1],B.stat$Min.pt[,2],B.stat$Min.pt[,3],xo=B.stat$X,yo=B.stat$Y,duplicate='mean')$z
C.stat$Min.interp<- interp(C.stat$Min.pt[,1],C.stat$Min.pt[,2],C.stat$Min.pt[,3],xo=C.stat$X,yo=C.stat$Y,duplicate='mean')$z
D.stat$Min.interp<- interp(D.stat$Min.pt[,1],D.stat$Min.pt[,2],D.stat$Min.pt[,3],xo=D.stat$X,yo=D.stat$Y,duplicate='mean')$z
E.stat$Min.interp<- interp(E.stat$Min.pt[,1],E.stat$Min.pt[,2],E.stat$Min.pt[,3],xo=E.stat$X,yo=E.stat$Y,duplicate='mean')$z
FF.stat$Min.interp<- interp(FF.stat$Min.pt[,1],FF.stat$Min.pt[,2],FF.stat$Min.pt[,3],xo=FF.stat$X,yo=FF.stat$Y,duplicate='mean')$z
G.stat$Min.interp<- interp(G.stat$Min.pt[,1],G.stat$Min.pt[,2],G.stat$Min.pt[,3],xo=G.stat$X,yo=G.stat$Y,duplicate='mean')$z
H.stat$Min.interp<- interp(H.stat$Min.pt[,1],H.stat$Min.pt[,2],H.stat$Min.pt[,3],xo=H.stat$X,yo=H.stat$Y,duplicate='mean')$z

# for Max map (linear interp)
A.stat$Max.interp<- interp(A.stat$Max.pt[,1],A.stat$Max.pt[,2],A.stat$Max.pt[,3],xo=A.stat$X,yo=A.stat$Y,duplicate='mean')$z
B.stat$Max.interp<- interp(B.stat$Max.pt[,1],B.stat$Max.pt[,2],B.stat$Max.pt[,3],xo=B.stat$X,yo=B.stat$Y,duplicate='mean')$z
C.stat$Max.interp<- interp(C.stat$Max.pt[,1],C.stat$Max.pt[,2],C.stat$Max.pt[,3],xo=C.stat$X,yo=C.stat$Y,duplicate='mean')$z
D.stat$Max.interp<- interp(D.stat$Max.pt[,1],D.stat$Max.pt[,2],D.stat$Max.pt[,3],xo=D.stat$X,yo=D.stat$Y,duplicate='mean')$z
E.stat$Max.interp<- interp(E.stat$Max.pt[,1],E.stat$Max.pt[,2],E.stat$Max.pt[,3],xo=E.stat$X,yo=E.stat$Y,duplicate='mean')$z
FF.stat$Max.interp<- interp(FF.stat$Max.pt[,1],FF.stat$Max.pt[,2],FF.stat$Max.pt[,3],xo=FF.stat$X,yo=FF.stat$Y,duplicate='mean')$z
G.stat$Max.interp<- interp(G.stat$Max.pt[,1],G.stat$Max.pt[,2],G.stat$Max.pt[,3],xo=G.stat$X,yo=G.stat$Y,duplicate='mean')$z
H.stat$Max.interp<- interp(H.stat$Max.pt[,1],H.stat$Max.pt[,2],H.stat$Max.pt[,3],xo=H.stat$X,yo=H.stat$Y,duplicate='mean')$z

# for ground data (linear interp)
my.ground <- 1
my.ground$X <- rep(A.stat$X,length(A.stat$X))
my.ground$Y <- rep(A.stat$Y,each=length(A.stat$Y))
my.ground$Z <- ground.xyz
my.ground$map <- ground
my.ground$interp<- interp(my.ground$X[!is.na(my.ground$Z)],my.ground$Y[!is.na(my.ground$Z)],my.ground$Z[!is.na(my.ground$Z)],xo=A.stat$X,yo=A.stat$Y,duplicate='mean')$z




library(pracma)
my.ground$interp <- Reshape(my.ground$interp,length(A.stat$X),length(A.stat$Y))
A.stat$Min.interp <- Reshape(A.stat$Min.interp,length(A.stat$X),length(A.stat$Y))
B.stat$Min.interp <- Reshape(B.stat$Min.interp,length(A.stat$X),length(A.stat$Y))
C.stat$Min.interp <- Reshape(C.stat$Min.interp,length(A.stat$X),length(A.stat$Y))
D.stat$Min.interp <- Reshape(D.stat$Min.interp,length(A.stat$X),length(A.stat$Y))
E.stat$Min.interp <- Reshape(E.stat$Min.interp,length(A.stat$X),length(A.stat$Y))
FF.stat$Min.interp <- Reshape(FF.stat$Min.interp,length(A.stat$X),length(A.stat$Y))
G.stat$Min.interp <- Reshape(G.stat$Min.interp,length(A.stat$X),length(A.stat$Y))
H.stat$Min.interp <- Reshape(H.stat$Min.interp,length(A.stat$X),length(A.stat$Y))
A.stat$Max.interp <- Reshape(A.stat$Max.interp,length(A.stat$X),length(A.stat$Y))
B.stat$Max.interp <- Reshape(B.stat$Max.interp,length(A.stat$X),length(A.stat$Y))
C.stat$Max.interp <- Reshape(C.stat$Max.interp,length(A.stat$X),length(A.stat$Y))
D.stat$Max.interp <- Reshape(D.stat$Max.interp,length(A.stat$X),length(A.stat$Y))
E.stat$Max.interp <- Reshape(E.stat$Max.interp,length(A.stat$X),length(A.stat$Y))
FF.stat$Max.interp <- Reshape(FF.stat$Max.interp,length(A.stat$X),length(A.stat$Y))
G.stat$Max.interp <- Reshape(G.stat$Max.interp,length(A.stat$X),length(A.stat$Y))
H.stat$Max.interp <- Reshape(H.stat$Max.interp,length(A.stat$X),length(A.stat$Y))



# test for MSE (Mean Square Error) surface comparison and RMSE (sqrt(MSE)) ====
my.surface <- abind(
  A.stat$Min.interp+(my.ground$interp-A.stat$Min.interp)-mean(A.stat$Min.interp+(my.ground$interp-A.stat$Min.interp),na.rm=TRUE),
  B.stat$Min.interp+(my.ground$interp-A.stat$Min.interp)-mean(B.stat$Min.interp+(my.ground$interp-A.stat$Min.interp),na.rm=TRUE),
  C.stat$Min.interp+(my.ground$interp-A.stat$Min.interp)-mean(C.stat$Min.interp+(my.ground$interp-A.stat$Min.interp),na.rm=TRUE),
  D.stat$Min.interp+(my.ground$interp-A.stat$Min.interp)-mean(D.stat$Min.interp+(my.ground$interp-A.stat$Min.interp),na.rm=TRUE),
  E.stat$Min.interp+(my.ground$interp-A.stat$Min.interp)-mean(E.stat$Min.interp+(my.ground$interp-A.stat$Min.interp),na.rm=TRUE),
  FF.stat$Min.interp+(my.ground$interp-A.stat$Min.interp)-mean(FF.stat$Min.interp+(my.ground$interp-A.stat$Min.interp),na.rm=TRUE),
  G.stat$Min.interp+(my.ground$interp-A.stat$Min.interp)-mean(G.stat$Min.interp+(my.ground$interp-A.stat$Min.interp),na.rm=TRUE),
  H.stat$Min.interp+(my.ground$interp-A.stat$Min.interp)-mean(H.stat$Min.interp+(my.ground$interp-A.stat$Min.interp),na.rm=TRUE),
  along=3)


# Code to remove noises from the two spruce before further analyses. it only keeps data within ± (3*sd(Z))
my.limits <- c(sd(as.vector(my.surface[,,1]),na.rm=TRUE)*3,
               sd(as.vector(my.surface[,,2]),na.rm=TRUE)*3,
               sd(as.vector(my.surface[,,3]),na.rm=TRUE)*3,
               sd(as.vector(my.surface[,,4]),na.rm=TRUE)*3,
               sd(as.vector(my.surface[,,5]),na.rm=TRUE)*3,
               sd(as.vector(my.surface[,,6]),na.rm=TRUE)*3,
               sd(as.vector(my.surface[,,7]),na.rm=TRUE)*3,
               sd(as.vector(my.surface[,,8]),na.rm=TRUE)*3)
for (i in 1:dim(my.surface)[1]){
  for(j in 1:dim(my.surface)[2]){
    for(k in 1:8){
      if(!is.na(my.surface[i,j,k])){
        if(my.surface[i,j,k]<(-my.limits[k]) | my.surface[i,j,k]>my.limits[k]){
          my.surface[i,j,k] <- NA
        }
      }
    }
  }
}

# Normalize data.  (Zi - mean(Z))/sd(Z)
my.surface.norm <- abind(
  (my.surface[,,1]-mean(as.vector(my.surface[,,1]),na.rm=TRUE))/sd(as.vector(my.surface[,,1]),na.rm=TRUE),
  (my.surface[,,2]-mean(as.vector(my.surface[,,2]),na.rm=TRUE))/sd(as.vector(my.surface[,,2]),na.rm=TRUE),
  (my.surface[,,3]-mean(as.vector(my.surface[,,3]),na.rm=TRUE))/sd(as.vector(my.surface[,,3]),na.rm=TRUE),
  (my.surface[,,4]-mean(as.vector(my.surface[,,4]),na.rm=TRUE))/sd(as.vector(my.surface[,,4]),na.rm=TRUE),
  (my.surface[,,5]-mean(as.vector(my.surface[,,5]),na.rm=TRUE))/sd(as.vector(my.surface[,,5]),na.rm=TRUE),
  (my.surface[,,6]-mean(as.vector(my.surface[,,6]),na.rm=TRUE))/sd(as.vector(my.surface[,,6]),na.rm=TRUE),
  (my.surface[,,7]-mean(as.vector(my.surface[,,7]),na.rm=TRUE))/sd(as.vector(my.surface[,,7]),na.rm=TRUE),
  (my.surface[,,8]-mean(as.vector(my.surface[,,8]),na.rm=TRUE))/sd(as.vector(my.surface[,,8]),na.rm=TRUE),
  along=3)

# Calculate and plot MSE (Mean Square Error) ====
Comp.to.26mar <- array(data=NA,dim=dim(my.surface)-c(0,0,1))
my.MSE.avg <- rep(NA,times=dim(Comp.to.26mar)[3])
my.MSE <- rep(NA,times=dim(Comp.to.26mar)[3])
my.dates <-c("23 Sept","23 Oct","11 Nov","20 Dec","11 Jan","15 Feb","8 Mar","26 Mar") 

par(mfrow=c(3,3),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
for(i in 1:dim(Comp.to.26mar)[3]){
  Comp.to.26mar[,,i] <- ((my.surface.norm[,,i]-my.surface.norm[,,8])^2)
  my.MSE.avg[i] <- mean(as.vector(Comp.to.26mar[,,i]),na.rm=TRUE)
  image.plot(Comp.to.26mar[,,i],zlim=c(0,4),
             main=paste("MSE map",my.dates[i],"in comparison to 26 March",sep=" "))
}
plot(1:7,my.MSE.avg,main="Average MSE")


par(mfrow=c(1,1),oma = c( 1, 1, 1,1 ),mar=c(1.5,1.5,1.5,1.5))
boxplot(as.vector(Comp.to.26mar[,,1]),
        as.vector(Comp.to.26mar[,,2]),
        as.vector(Comp.to.26mar[,,3]),
        as.vector(Comp.to.26mar[,,4]),
        as.vector(Comp.to.26mar[,,5]),
        as.vector(Comp.to.26mar[,,6]),
        as.vector(Comp.to.26mar[,,7]),
        xlab=my.dates[1:7],ylim=c(0,4.5))

canopy.heigth <- (A.stat$Max.interp+(my.ground$interp-A.stat$Min.interp)-mean(A.stat$Max.interp+(my.ground$interp-A.stat$Min.interp),na.rm=TRUE))
plot((B.stat$Max.interp-B.stat$Min.interp),Comp.to.26mar[,,2])





# Save my.surface into a Matlab m-file ====
Detrended <- 1    # are the data detrended? I/O
writeMat("my.surface.mat",my_surface=my.surface,dx=dx,dy=dy,Detrended=Detrended)




