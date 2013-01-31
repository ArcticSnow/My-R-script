# VOXELIZATION of a point cloud
# Simon Filhol, 20 Decembre 2012

Voxel.ptcloud <- function(data,dx,dy,dz,Xlim,Ylim,Zlim){
  time <- Sys.time()
  # Create 3d Array
  x.res <- seq(from=Xlim[1],to=Xlim[2]-dx,by=dx)
  y.res <- seq(from=Ylim[1],to=Ylim[2]-dy,by=dy)
  z.res <- seq(from=Zlim[1],to=Zlim[2]-dz,by=dz)
  voxel <- array(data=NA,dim=c(length(x.res),length(y.res),length(z.res)))

  
  for(i in x.res){
    m <- 1
    ind1 <- ((data[,1]>=i)&data[,1]<(i+dx))
    ifelse(length(ind1[ind1==TTRUE]>=1),{
      inter1 <- data[ind1,]
      
      for(j in y.res){
        n <- 1
        ind2 <- (inter1[,2]>=j)&(inter1[,2]<(j+dy))
        ifelse(length(ind2[ind2==TRUE]>=1),{
          inter2 <- inter1[ind2,]
          
              for(k in z.res){
                o <- 1
                ind3 <- (inter2[,3]>=k)&(inter2[,3]<(k+dz))
                ifelse(length(ind3[ind3==TRUE]>=1),{
                  inter3 <- inter1[ind3,]
                  
                  
                voxel[m,n,o] <- length(inter3[,1])
              
              
                },
              {voxel[m,n,o] <- 0})
                o <- o+1
              }
        },{voxel[m,n,] <- rep(0,length(z.res))})
      }
    },{voxel[m,,] <- matrix(0,nrow=length(y.res),ncol=length(z.res))})
  }
          
  
  
  
  
  print(Sys.time()-time)
}















