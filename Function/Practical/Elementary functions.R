# Compilation of uselful function for point cloud analysis
# Simon Filhol



# Point density function ========================================================
# by Simon Filhol, 30 April 2012
# last modification: 2May12
# Function that generate a desnity map of a rectangle region


Point_Density <- function(data,dx,dy,Xlim,Ylim){
  library(MASS)
  
  
  #limit <- c(min(data[,1]),max(data[,1]),min(data[,2]),max(data[,2]))
  
  #density <- kde2d(data[,1],data[,2],n=res,lims = c(Xlim,Ylim))
  
  x.res <- seq(from=Xlim[1],to=Xlim[2]-dx,by=dx)
  y.res <- seq(from=Ylim[1],to=Ylim[2]-dy,by=dy)
  z.density <- matrix(NA,nrow=length(x.res),ncol=length(y.res))
  
  k <- 1
  for(i in x.res){
    m <- 1
    for(j in y.res){
      ind <- which((data[,1]>=i)&(data[,1]<(i+dx))&(data[,2]>=j)&(data[,2]<(j+dy)))
      z.density[k,m] <- length(ind)/(dx*dy*10*10)
      m <- m+1
    } 
    k <- k+1
  }
  
  density <- list(x=x.res+dx/2,y=y.res+dy/2,z=z.density)
  
  print(cat('min and max density =',c(min(density$z),max(density$z))))
  
  layout(t(c(1,1,1,1,1,2,2)))
  image(density$x,density$y,log(density$z),xlab="x (m)", ylab="y (m)", main = "Point density of raw data in pts/dm2" )
  contour(density,add=TRUE,labcex=1.2)
  
  #   hist(density$z,breaks=seq(from=0,to=max(density$z)+100, by= 50))
  hist(log(density$z), main = "Histogram of point density", xlab="Point density")
  
  return(density)
}
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''
## Example:
# Point_Density(myMat,20)

# Truncate Function ======================================================
# by Simon Filhol, 30 April 2012

# Function to select a rectangle of data over X and Y
# my.data mus be a n*3 matix (XYZ)

Truncate_XY <- function(my.data,Xlim,Ylim){
  
  # Another possility is to use the locations.inside() function
  # of the geoR package
  Xmin <- Xlim[1]
  Xlength <- Xlim[2]-Xlim[1]
  Ymin <- Ylim[1]
  Ylength <- Ylim[2]-Ylim[1]
  ind <- which((my.data[,1]>=Xmin) & (my.data[,1]<=Xmin+Xlength)&(my.data[,2]>=Ymin) & (my.data[,2]<=Ymin+Ylength))
  
  
  return(My.sample <- my.data[ind,] )
  
}

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''
# # EXAMPLE:
# #
# myMat<-matrix(runif(10000*3), ncol=3)
# plot(myMat[,1],myMat[,2])
# system.time(Select <- Truncate_XY(myMat,Xmin=0,Ymin=0,Xlength=1,Ylength=1))
# #
#
# # Xmin <- 0.2
# # Ymin <- 0.4
# # Xlength <- 0.2
# # Ylength <- 0.3
# #
# #
# # which <- rep(FALSE,times=dim(myMat)[1])
# # for(i in 1:dim(myMat)[1] ){
# #   if((myMat[i,1]>=Xmin) & (myMat[i,1]<=Xmin+Xlength)){
# #     if((myMat[i,2]>=Ymin) & (myMat[i,2]<=Ymin+Ylength)){
# #       which[i] <- TRUE
# #     }
# #   }
# # }
# #
# # Select<- myMat[which,]
# plot(Select[,1],Select[,2])

# Subsampling function ============================================================
# by Simon Filhol, 30 April 2012

# function which subsample according to the technique indicated
# data must be a n*3 matrix
# dx and dy are scalar
# technic can be "min", "max", or complete code with switch() statment.
# cutoff disregards data in column if there are less point than the cutoff value.

Subsample_pts <-  function(data,dx,dy,technic,cutoff){
  
  
  Yres <- round((max(data[,2])-min(data[,2]))/dy)
  Xres <- round((max(data[,1])-min(data[,1]))/dx)
  a <- length(seq(from=round(min(data[,1])),to=max(data[,1]),by=dx))
  b <- length(seq(from=round(min(data[,2])),to=max(data[,2]),by=dy))
  output <- matrix(NA,a*b,3)
  
  switch(technic,
         min={
           k <- 1
           for(i in seq(from=round(min(data[,1])),to=max(data[,1]),by=dx)){
             for(j in seq(from=round(min(data[,2])),to=max(data[,2]),by=dy)){
               ind <- which((data[,1]>=i)&(data[,1]<(i+dx))&(data[,2]>=j)&(data[,2]<(j+dy)))
               if(length(ind)<cutoff){
                 output[k,] <- c(NA,NA,NA)
                 rm(ind)
               }
               else{
                 sub.sample <- data[ind,]
                 ind2 <- which(sub.sample[,3]==min(sub.sample[,3]))
                 if(length(ind2>1)){ind2 <- sample(ind2,1)}
                 output[k,] <- sub.sample[ind2,]
                 
               }
               k <- k+1
             }
           }
           ind3 <- which(is.finite(output[,2]))
           output <- output[ind3,]
           return(output)
         },
         max={
           k <- 1
           for(i in seq(from=round(min(data[,1])),to=max(data[,1]),by=dx)){
             for(j in seq(from=round(min(data[,2])),to=max(data[,2]),by=dy)){
               ind <- which((data[,1]>=i)&(data[,1]<(i+dx))&(data[,2]>=j)&(data[,2]<(j+dy)))
               if(length(ind)<cutoff){
                 output[k,] <- c(NA,NA,NA)
                 rm(ind)
               }
               else{
                 sub.sample <- data[ind,]
                 
                 ind2 <- which(sub.sample[,3]==max(sub.sample[,3]))
                 if(length(ind2>1)){ind2 <- sample(ind2,1)}
                 output[k,] <- sub.sample[ind2,]
                 
               }
               k <- k+1
             }
           }
           ind3 <- which(is.finite(output[,2]))
           output <- output[ind3,]
           return(output)
         },
         stop("This technique does not exist, only 'min' or 'max', or add new code")
         )
}

##'''''''''''''''''''''''''''''''''''''''''''''''''''''''
# EXAMPLE
# data <- matrix(runif(10*3), ncol=3)
# smin <- Subsample_pts(data,.1,.1,"min")
# plot(smin[,1],smin[,2])

# my.plot.results==================================================================================
# by Margaret Short, adapted by Simon Filhol, 1st May 2012


my.plot.results <- function( my.lonlats,my.values,my.dxdy,str,Xlim,Ylim,leg.titl )
{if(length(my.values==2)){
  layout(1)
  Xmin=Xlim[1]
  Xmax=Xlim[2]
  Ymin=Ylim[1]
  Ymax=Ylim[2]
  my.lons <- seq(Xmin,Xmax,my.dxdy)
  my.lats <- seq(Ymin,Ymax,my.dxdy)
  
  my.grays <- gray( 5:59/63 )
  my.grays[length(my.grays)] <- "white"
  
  borders <- cbind(c(Xmin,Xmax,Xmax,Xmin),c(Ymin,Ymin,Ymax,Ymax)) 
  # Find which points are within the borders (resulting values
  # can take 0=outside, 1=inside, 2=on edge)
  in.border <- point.in.polygon(my.lonlats[,1], my.lonlats[,2],
                                borders[,1],borders[,2])
  
  
  my.results <- matrix(NA, nrow=length(my.lons), ncol=length(my.lats))
  my.results[in.border!=0] <- my.values
  
  par(mar=c(4, 4, 4, 4))
  image(my.lons, my.lats, my.results, col=my.grays,
        main=str, cex.main=1.5, xlab="x (m)",ylab="Y (m)")
  
  contour(my.lons, my.lats, my.results, add=T, labcex=1.2)
  legend.col(col=gray( 5:59/63 ),lev=my.results,title=leg.titl)
  # default labcex is 0.6
  return(my.results)
}
}

# Frequentist Kriging function ==================================================================================
# Data.entry - is a XYZ list of points for kriging
# Xlim - is a vector indicating the limit of the area of interest in the x-direction
# Ylim - is a vector indicating the limit of the area of interest in the x-direction
# my.krig.dxdy - is the resolution of the output product


Grd_Freq_krig <- function(data.entry,Xlim,Ylim,dx,dy,my.krig.dxdy){
  
  data <- matrix(c(data.entry[,1],data.entry[,2],data.entry[,3]),ncol=3)
  data <- Truncate_XY(data,Xlim,Ylim)
  
  res=(Xlim[2]-Xlim[1])/dx
  my.pt.density <- Point_Density(data,res,Xlim,Ylim)
  smin <- Subsample_pts(data,dx,dy,"min")
  plot(smin[,1],smin[,2])

  
  # Method 1: linear interpolation
  # Interpolqation using Akima package
  library(akima)
  my.xo <- seq(from=Xlim[1],to=Xlim[2],by=my.krig.dxdy)
  my.yo <- seq(from=Ylim[1],to=Ylim[2],by=my.krig.dxdy)
  my.interp <- interp(smin[,1],smin[,2],smin[,3],my.xo,my.yo,linear=TRUE)
  my.grays <- gray(5:59/64)
  layout(1)
  image(my.interp,col=my.grays)
  contour(my.interp,add=TRUE,labcex=1.5)
  
  # MEHTOD 2: frequentist Kriging
  library(geoR)
  
  my.geodata <- as.geodata(matrix(c(smin[,1],smin[,2],smin[,3]),ncol=3))
  
  # # Detrend smin before processing
  # my.trend <- trend.spatial("1st",my.geodata)
  # d<- my.geodata
  # 
  # if(dim(my.trend)[2]==3){
  #   d$data <- my.geodata$data - (my.trend[,1]+(my.trend[,2]*my.geodata$coords[,1])+((my.trend[,3])*my.geodata$coords[,2]))
  # }else{
  #   d$data <-  my.geodata$data-(my.trend[,1]+(my.trend[,2]*my.geodata$coords[,1])+((my.trend[,3])*my.geodata$coords[,2])+(my.trend[,4]*(my.geodata$coords[,1])^2)+(my.trend[,5]*(my.geodata$coords[,2])^2)+(my.trend[,6]*my.geodata$coords[,1]*my.geodata$coords[,2]))
  # }
  # Robust empirical semivariogram
  
  my.robust.vario.directional <- variog4(my.geodata,trend="2nd",estimator.type="modulus",direction=c(0,pi/4,pi/2,3*pi/4),tolerance=pi/8, max.dist=1.5)
  plot(my.robust.vario.directional)
  
  my.robust.vario <- variog(my.geodata,trend="2nd",estimator.type="modulus",max.dist=1.5)
  plot(my.robust.vario)
  print("What semivariogram to use?")
  switch(menu(c("Exponential","Spherical","Gaussian")),
         c={
           c <- "exponential"},
         c={
           c <- "spherical"},
         c={
           c <- "gaussian"}
         )
 
  # covar.param <- readline("Choose ini.cov.pars, ex:c(partial sill, range): ")
  my.var.fit <- variofit(my.robust.vario,ini.cov.pars=c(0.005,0.025),cov.model=c,fix.nugget=FALSE,nugget=0.005,weights="equal",max.dist=1.5)
  lines(my.var.fit)
  my.tausq <- my.var.fit$nugget
  my.sigsq <- my.var.fit$cov.pars[1]
  my.phi <- my.var.fit$cov.pars[2]
  
  my.grid <- pred_grid(Xlim,Ylim,by=my.krig.dxdy)
  
  my.kr.obj <- krige.control(type.krige="OK",cov.model=c,cov.pars=c(my.sigsq,my.phi),nugget=my.tausq)
  my.kr.results <- krige.conv(my.geodata,locations=my.grid,krige=my.kr.obj)
  
  # plotting kriging code
  
  my.kr.sd <- sqrt(my.kr.results$krige.var)
  my.kr.pred <- my.kr.results$predict
  
  my.results <- my.plot.results( my.grid,my.kr.pred, my.krig.dxdy,"Universal Kriging",Xlim=Xlim,Ylim=Ylim)
  
  All.resutls <- list(my.pt.density)
  return(All.results) 
}

# Multiple plot function ====
# 
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Fit Plane function ====

FitPlane <- function(XYZ){
  # fitplane function adapted from Matlab to R:  
  
  #    FITPLANE - solves coefficients of plane fitted to 3 or more points
  #   
  #    Usage:   B = fitplane(XYZ)
  #   
  #    Where:   XYZ - Npts*3 array of xyz coordinates to fit plane to.   
  #                   If Npts is greater than 3 a least squares solution 
  #                   is generated.
  #   
  #    Returns: Bs   - 4x1 array of plane coefficients in the form
  #                   b(1)*X + b(2)*Y +b(3)*Z + b(4) = 0
  #                   The magnitude of B is 1.
  #                   To get gtound surface detrend, just estimate the 
  #                   residuals which are equal to
  #                   Residual = b(1)*X + b(2)*Y +b(3)*Z + b(4)
  
  dimen  <-  dim(XYZ)    
  
  if (dimen[2] !=3){
    stop('data is not 3D')
  }
  
  if (dimen[1] < 3){
    stop('too few points to fit plane')
  }
  
  # Set up constraint equations of the form  AB = 0,
  # where B is a column vector of the plane coefficients
  # in the form   b(1)*X + b(2)*Y +b(3)*Z + b(4) = 0.
  
  A  <-  cbind(XYZ, matrix(1,nrow=dimen[1] )) # Build constraint matrix
  
  ss <- svd(A)        # Singular value decomposition.
  Bs  <-  ss$v[,4]       # Solution is last column of v.
  return(Bs)             
}

# Load point cloud ====
# Simon Filhol, 28 July 2012
# Load point cloud files interactively, any 3 columns file like: .xyz or .txt file

LoadPointCloud <- function(Trans,file){
  
  ifelse(missing(file),{my.file <- file.choose()},{my.file <- file})
  #print(file)
  # Load data into 3 column matrix
  raw.data <- read.table( my.file, quote="\"")
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
    my.info <- list(File=my.file,Translation=my.trans.xyz,Data= my.data)
  }
  else{
    my.info <- list(File=my.file,Data= my.data) 
  }
  return(my.info)
}

# Multiplot function ====
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Data Prepare function ====
Data.prepare <- function(my.data,xlim,ylim,dx,dy,return.xyz){
  xyz <- my.data$Data
  translation <- my.data$Translation
  rm(my.data)
  
  xyz <- xyz[xyz[,1]>=xlim[1] & xyz[,1] <=xlim[2],]
  xyz <- xyz[xyz[,2]>=ylim[1] & xyz[,2] <=ylim[2],]
  source("/Users/simonfilhol/github/local/My_R_script/Function/Practical/Basic_statmap_ptcloud.r")
  stat <- Basic.statmap.ptcloud(xyz,dx,dy,xlim,ylim)
  if(missing(return.xyz)){return.xyz=FALSE}
  if(return.xyz==FALSE){
    rm(xyz)
    All <- list(Stat= stat,
                Trans=translation)}
  else{
    All <- list(Stat= stat,
                Trans=translation,
                xyz=xyz)
  }
  return(All)
}
