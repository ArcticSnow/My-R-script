# Simon Filhol, 1st august 2012
#
# This function return a list of statistics about the 
# vertical distribution of points in a dx*dy vertical column
# 
# Input:
#   - data - must be a 3 columns vector, XYZ
#   - dx, dy - are size of the column to derive statistics from
#   - Xlim, Ylim - are outer limit of the area of interst, there must be a vector of length 2
# 
# Output:
#   - return a list with statistics stored in a 2D matrix. run summary(...) 
#     to see all variable stored in the list
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

Basic.statmap.ptcloud <- function(data,dx,dy,Xlim,Ylim){
  
   # if(Xlim[1]<min(data[,1]) | Xlim[2]>max(data[,1]) | Ylim[1]<min(data[,2])  | Ylim[2]>max(data[,2]) ){
     # stop("Limits are wider than actual data")
   # }
  
  
  data <- data[data[,1]>=Xlim[1] & data[,1] <=Xlim[2],]
  data <- data[data[,2]>=Ylim[1] & data[,2] <=Ylim[2],]
  
  print(dy)
  # Initialize 
  x.res <- seq(from=Xlim[1],to=Xlim[2]-dx,by=dx)
  y.res <- seq(from=Ylim[1],to=Ylim[2]-dy,by=dy)
  z.density <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
  z.min <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
  z.max <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
  z.mean <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
  z.std <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
  z.quantile <- array(NA,dim=c(length(x.res),length(y.res),3))
  z.pt.loc.min <- matrix(NA,nrow=(length(x.res)+1)*(length(y.res)+1),ncol=3 )
  z.pt.loc.max <- matrix(NA,nrow=(length(x.res)+1)*(length(y.res)+1),ncol=3 )
  
  
  print(length(x.res)*length(y.res))
  # Loop to split data in pixels dx*dy and extract statistique parameters
  time <- Sys.time()
  
  k <- 1
  n <- 1
  for(i in x.res){
    m <- 1
    ind1 <- ((data[,1]>=i)&data[,1]<(i+dx))
    
    ifelse(length(ind1[ind1==T]>=1),{
      inter1 <- data[ind1,]
        for(j in y.res){
        ind2 <- (inter1[,2]>=j)&(inter1[,2]<(j+dy))
        ifelse(sum(ind2)>=1,{
          inter2 <- inter1[ind2,3]
        
          z.density[k,m] <- length(inter2)/(dx*dy)
          z.min[k,m] <- min(inter2,na.rm=T)
          z.max[k,m] <- max(inter2,na.rm=T)
          z.mean[k,m] <- mean(inter2,na.rm=T)
          z.std[k,m] <- sd(inter2,na.rm=T)
          z.quantile[k,m,] <- quantile(inter2,probs=c(0.025,0.5,0.975),na.rm=TRUE)
          inter2.5 <- inter1[ind2,1:3]
         
          if (length(inter2.5)==3){
          	z.pt.loc.min[n,] <-inter2.5
          	z.pt.loc.max[n,] <-inter2.5
          }
          else{
            inter3 <- inter2.5[inter2.5[,3]== z.min[k,m],]
            inter4 <- inter2.5[inter2.5[,3]== z.max[k,m],]
            if(!is.null(dim(inter3))){inter3 <- inter3[1,]}
            if(!is.null(dim(inter4))){inter4 <- inter4[1,]}
          	z.pt.loc.min[n,] <- inter3
          	z.pt.loc.max[n,] <-inter4
          }
          },{
          z.density[k,m] <- NA
          z.min[k,m] <- NA
          z.max[k,m] <- NA
          z.mean[k,m] <- NA
          z.std[k,m] <- NA
          z.quantile[k,m,] <- c(NA,NA,NA) 
          z.pt.loc.min[n,] <-c(NA,NA,NA) 
          z.pt.loc.max[n,] <-c(NA,NA,NA) 
        })
        m <- m+1
        n <- n+1
        
      }
      data <- data[!ind1, ]
    },{
      z.density[k,] <- NA
      z.min[k,] <- NA
      z.max[k,] <- NA
      z.mean[k,] <- NA
      z.std[k,] <- NA
      z.quantile[k,,] <- c(NA,NA,NA)
      z.pt.loc.min[n,] <-c(NA,NA,NA) 
      z.pt.loc.max[n,] <-c(NA,NA,NA)
    })
    
    k <- k+1
    n <- n+1
  }
print(Sys.time()-time)

z.pt.loc.min <- z.pt.loc.min[!is.na(z.pt.loc.min[,3]),] 
z.pt.loc.max <- z.pt.loc.max[!is.na(z.pt.loc.max[,3]),]  
  
  
  #will return All
All <- list(X.coord=x.res+dx/2,    # need to verify if output for X.coord is right. Might need to run the function map2xyz here
			Y.coord=y.res+dy/2,
            Density=z.density,
            Min=z.min,
            Max=z.max,
            Mean=z.mean,
            Sd=z.std,
            Quantiles=z.quantile,
            Min.pt=z.pt.loc.min,
            Max.pt=z.pt.loc.max,
            X=x.res,
            Y=y.res
            
)
return(All)
}
