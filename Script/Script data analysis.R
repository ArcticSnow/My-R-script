
setwd("F:/Phd/Course/Spatial Stat/Project")
# Load Data and prepare for analysis
Stat_project2 <- read.table("F:/Phd/Course/Spatial Stat/Project/Stat_project2.xyz", quote="\"")
source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/ElemFunction/R/Elementary functions.r")
source("C:/Documents and Settings/sfilhol/My Documents/GitHub/My-R-script/Function/Legend colorscale function.r")

#Grd_Freq_krig(Stat_project2,Xlim=c(0,2),Ylim=(c(-12,-10)),dx=.1,dy=.1,my.krig.dxdy=.01)







# Initial paramters
Xlim <- c(0,5)
Ylim <- c(-12,-8)
dx <- .1
dy <- .1
my.krig.dxdy <- .01


data <- matrix(c(Stat_project2[,1],Stat_project2[,2],Stat_project2[,3]),ncol=3)
data <- Truncate_XY(data,Xlim,Ylim)


a=.5
plot(Truncate_XY(data,c(a,a+.1),Ylim)[,2],Truncate_XY(data,c(a,a+.1),Ylim)[,3],xlab='Distance (m)',ylab='Elevation (m)')



#-------------------------------------------------
# Data exploratory

layout(1,1,1,2)
plot(data[,1],data[,2],pch=42,xlim=c(0,2),cex=.5,xlab="x (m)", ylab="Y (m)",main="Point cloud elevation in 2*2m area, (m)",col=terrain.colors(30)[data[,3]])
legend.col(col=terrain.colors(30)[data[,3]],lev=data[,3],"Elevation (m)")



res=(Xlim[2]-Xlim[1])/dx
my.ptdensity <- Point_Density(data,dx,dy,Xlim,Ylim)

hist(density$z[,1:20])


# data <- matrix(runif(1000*3), ncol=3)
smin <- Subsample_pts(data,dx,dy,"min",10)
layout(1)
par(mar=c(4, 4, 4, 4))
plot(smin[,1],smin[,2],mar=c(4,4,4,4),pch=".",cex=10,xlab="x (m)",ylab="Y (m)",main="Subsample of point in lowest \nelevation in 10*10cm grid" ,col=terrain.colors(30)[smin[,3]])
legend.col(col=terrain.colors(30)[smin[,3]],lev=smin[,3],"Elevation (m)")

boxplot(smin[,3])


#-------------------------------------------------
# Method 1: linear interpolation
# Interpolqation using Akima package
library(akima)

my.xo <- seq(from=Xlim[1],to=Xlim[2],by=my.krig.dxdy)
my.yo <- seq(from=Ylim[1],to=Ylim[2],by=my.krig.dxdy)
my.interp <- interp(smin[,1],smin[,2],smin[,3],my.xo,my.yo,linear=TRUE)
my.grays <- gray(5:59/64)
layout(1)
image(my.interp,col=my.grays, xlab="x (m)",ylab="Y (m)",main="Linear interpolation")
contour(my.interp,add=TRUE,labcex=1.5)

#-------------------------------------------------
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

my.robust.vario.directional <- variog4(my.geodata,trend="2nd",estimator.type="modulus",direction=c(0,pi/4,pi/2,3*pi/4),tolerance=pi/8)
my.robust.vario <- variog(my.geodata,trend="2nd",estimator.type="modulus")
plot(my.robust.vario.directional,xlab="lag (m)",ylab="semivariance",main="Empirical directional semivariogram" )
plot(my.robust.vario,xlab="lag (m)",ylab="semivariance",main="Empirical semivariogram")

#function plotting shuffling test for significance of using a spatial mnodel.
Test.sp.model(my.robust.vario,my.geodata,"2nd","Random shuffle test, 100 simulations")

my.var.fit <- variofit(my.robust.vario,ini.cov.pars=c(0.005,0.025),cov.model="exponential",fix.nugget=FALSE,nugget=0.005,weights="equal",max.dist=1.5)
lines(my.var.fit)
my.tausq <- my.var.fit$nugget
my.sigsq <- my.var.fit$cov.pars[1]
my.phi <- my.var.fit$cov.pars[2]

my.grid <- pred_grid(Xlim,Ylim,by=my.krig.dxdy)

my.kr.obj <- krige.control(type.krige="OK",cov.model="exponential",cov.pars=c(my.sigsq,my.phi),nugget=my.tausq)
my.kr.results <- krige.conv(my.geodata,locations=my.grid,krige=my.kr.obj)

# plotting kriging code

my.kr.sd <- sqrt(my.kr.results$krige.var)
my.kr.pred <- my.kr.results$predict
my.kr.sd <- sqrt(my.kr.results$krige.var)

postscript("2by2_krig.ps",horiz=T)
par(mar=c(4, 4, 4, 4))
par(mfrow=c(2,2))
my.plot.results( my.grid,my.kr.pred, my.krig.dxdy,"Universal Kriging",Xlim=Xlim,Ylim=Ylim,"Elevation (m)")
my.plot.results(my.grid,2*1.96*my.kr.sd,my.krig.dxdy,"95% Prediction Interval",Xlim,Ylim,"Prediction interval (m)")
my.plot.results(my.grid,(my.kr.pred-1.96*my.kr.sd),my.krig.dxdy,"Low 2.5",Xlim,Ylim,"Elevation (m)")
my.plot.results(my.grid,(my.kr.pred+1.96*my.kr.sd),my.krig.dxdy,"High 97.5",Xlim,Ylim,"Elevation (m)")
graphics.off()

#######################################################################
####################### BAYESIAN ANALYSIS ##############################



library(geoR)



print(names(my.geodata))
# strata sample lat long tcatch prerec recruits lgcatch

# create centered longitudes and latitudes:
c.lons <- my.geodata$coords[,1] - mean(my.geodata$coords[,1])
c.lats <- my.geodata$coords[,2] - mean(my.geodata$coords[,2])

# my.geodata <- as.geodata(
#   cbind(c.lons,c.lats,my.geodata$lgcatch),
#   coords.col=1:2, data.col=3 )

bdr <- chull( my.geodata$coords )
my.geodata$borders <- cbind(c.lons[bdr],c.lats[bdr])

# plot(my.geodata) # so you can see data and lons/lats & border

my.prior.control <- prior.control(
  beta.prior="normal",
  beta=c(0,0,0),  # prior mean for the 3 betas
  beta.var.std= diag(10000,3), # prior variances of 10000 for the 3 betas
  sigmasq.prior="reciprocal",
  
  phi.prior="exponential",
  phi=5, # value of phi = prior mean for phi
  
  tausq.rel.prior="uniform",
  tausq.rel.discrete=seq(from=0,to=.20,by=.02))

my.model.control <- model.control(trend.d="1st",
                                  trend.l="1st", cov.model="exponential")

my.output.control <- output.control( 10000 ) # this many MCMC iterations

my.dxdy <- 0.06

# CHANGE BOUNDARY OF REGION
 minlon <- Xlim[1]-mean(my.geodata$coords[,1]) # after centering
 maxlon <- minlon+Xlim[2]-Xlim[1]
 minlat <- Ylim[1]-mean(my.geodata$coords[,2])
maxlat <-  minlat+Ylim[2]-Ylim[1]
 # my.grid <- pred_grid(Xlim-mean(my.geodata$coords[,1]),Ylim--mean(my.geodata$coords[,2]),by=my.dxdy)
my.grid <- pred_grid( c(minlon,maxlon), c(minlat,maxlat), by=my.dxdy )

my.gr.in <- locations.inside(my.grid, my.geodata$borders)

my.pred.locs <- as.matrix(my.gr.in)

my.bayes.results <- krige.bayes(my.geodata,
                                locations=my.pred.locs,
                                model=my.model.control,
                                prior=my.prior.control,
                                output=my.output.control)

print(names(my.bayes.results$posterior$sample))

# R code for various plots:

tmp000 <- my.bayes.results$posterior$sample$beta0

my.which <- seq( from=1,to=length(tmp000), by=4 )
# so we are thinning by excluding 3 out of 4 iterations


my.beta0.draws <- my.bayes.results$posterior$sample$beta0[my.which]
my.beta1.draws <- my.bayes.results$posterior$sample$beta1[my.which]
my.beta2.draws <- my.bayes.results$posterior$sample$beta2[my.which]

my.sigsq.draws     <- my.bayes.results$posterior$sample$sigmasq[my.which]
my.phi.draws       <- my.bayes.results$posterior$sample$phi[my.which]
my.tausq.rel.draws <- my.bayes.results$posterior$sample$tausq.rel[my.which]
my.tausq.draws     <- my.tausq.rel.draws * my.sigsq.draws

library(coda)

postscript("my.geodata_Bayes.ps",horiz=FALSE)
par(mfrow=c(4,2))
my.mcmc.output <- mcmc( cbind(
  my.beta0.draws,  my.beta1.draws,  my.beta2.draws,
  my.sigsq.draws,my.phi.draws,my.tausq.draws) )

my.summary <- function(vec,str)
{
  print(summary(vec))
  plot(1:length(vec), vec, main=str, type='l')
}

if(FALSE) { # debugging code, don't need to run
  nnn <- length(my.beta0.draws)
  my.summary(my.beta0.draws,"beta0")
  my.summary(my.beta1.draws,"beta1")
  my.summary(my.beta2.draws,"beta2")
  my.summary(my.sigsq.draws,"sigsq")
  my.summary(my.tausq.draws,"tausq")
  my.summary(my.phi.draws,"phi")
}
plot(my.mcmc.output)

par(mfrow=c(1,1))
crosscorr.plot(my.mcmc.output)
graphics.off()

my.plot.results <- function(
  minlon,maxlon, minlat,maxlat, dxdy,
  my.values, my.label, my.borders,
  add.to.lons, add.to.lats )
{
  # my.values is for points inside the border(s)
  
  my.grid <- pred_grid( 
    add.to.lons+c(minlon,maxlon),
    add.to.lats+c(minlat,maxlat), by=dxdy )
  
  my.grays <- gray( 5:59/63 )
  my.grays[length(my.grays)] <- "white"
  
  # Find which points are within the borders (resulting values
  # can be: 0=outside, 1=inside, 2=on edge)
  
  in.border <- point.in.polygon(my.grid[,1], my.grid[,2],
                                my.borders[,1]+add.to.lons,
                                my.borders[,2]+add.to.lats)
  
  my.lons <- seq(from=minlon,to=maxlon,by=dxdy) + add.to.lons
  my.lats <- seq(from=minlat,to=maxlat,by=dxdy) + add.to.lats
  
  my.results <- matrix(NA, nrow=length(my.lats), ncol=length(my.lons))
  my.results[in.border!=0] <- my.values
  
  image(my.lons, my.lats, my.results, col=my.grays,
        main=my.label, cex.main=1.5)
  
  contour(my.lons, my.lats, my.results, add=T, labcex=1.2)
  # default labcex is 0.6
}


postscript("my.geodata_Bayes_2.ps",horiz=TRUE)
par(mfrow=c(2,2))

add.to.lons <- 0; add.to.lats <- 0
my.plot.results(
  minlon,maxlon, minlat,maxlat, my.dxdy,
  my.bayes.results$predictive$mean,
  "Bayesian kriged surface",
  my.geodata$borders,
  add.to.lons,add.to.lats )

my.plot.results(
  minlon,maxlon, minlat,maxlat, my.dxdy,
  sqrt(my.bayes.results$predictive$variance),
  "variance",
  my.geodata$borders,
  add.to.lons, add.to.lats )

# the next two calls to 'my.plot.results' show
# how we can use uncentered longitudes & latitudes
# in our plots by adding what we earlier subtracted
# when centering:

add.to.lons <- mean(my.geodata$coords[,1])
add.to.lats <- mean(my.geodata$coords[,2])
my.plot.results(
  minlon,maxlon, minlat,maxlat, my.dxdy,
  my.bayes.results$predictive$mean,
  "Bayesian kriged surface",
  my.geodata$borders,
  add.to.lons,add.to.lats )

my.plot.results(
  minlon,maxlon, minlat,maxlat, my.dxdy,
  sqrt(my.bayes.results$predictive$variance),
  "variance",
  my.geodata$borders,
  add.to.lons, add.to.lats )


#dim(my.bayes.results$predictive$simulations)
#one column per MCMC iteration.

graphics.off()

summary(my.mcmc.output)

###################################
# can save samples out to disk for later use:

post <- my.bayes.results$posterior$sample

temp <- cbind(
  post$beta0, post$beta1,
  post$beta2, post$tausq.rel * post$sigmasq,
  post$sigmasq,post$phi)

fname <- "my_bayes_parm_samples.txt"
write( "beta0 beta1 beta2 tausq sigmasq phi",
       file=fname )
write( t(temp), file=fname, ncol=ncol(temp), append=TRUE )

#############################################
# we'll need to be able to calculate -2LogL in order
# to calculate DIC, so create a function for this:

# assumes existence of distance matrix, distmat.

neg2logL.no.trend <- function(mu,tausq,sigsq,phi, yy)
{  # exponential variogram -- need other versions
  # for gaussian variogram, etc.
  
  n <- length(yy)
  y.minus.mu <- yy - mu
  
  Sigma <- sigsq*exp( -distmat/phi ) + diag(tausq,n)
  
  my.eigen <- eigen(Sigma)
  
  if( min(my.eigen$values) < 0 ) {
    print("mu tausq sigsq phi")
    print(c(mu,tausq,sigsq,phi))
    print(sort(my.eigen$values))
    stop("here")
  }
  
  inv.evals <- 1/sqrt( my.eigen$values )
  term1 <- y.minus.mu %*% my.eigen$vectors %*% diag(inv.evals)
  term1 <- sum( term1^2 )
  
  #term1 <- y.minus.mu %*% solve(Sigma) %*% y.minus.mu
  
  LogL <- -n*0.5*log(2*pi) - 0.5*term1
  return( -2*LogL )
}

neg2logL.quadratic.trend <- function(
  betas, tausq,sigsq,phi, yy, lons,lats)
{  # exponential variogram -- need other versions
  # for gaussian variogram, etc.
  
  n <- length(yy)
  y.minus.mu <- yy - (betas[1] + 
    betas[2]*lons + betas[3]*lats)
  
  Sigma <- sigsq*exp( -distmat/phi ) + diag(tausq,n)
  
  my.eigen <- eigen(Sigma)
  
  if( min(my.eigen$values) < 0 ) {
    print("betas tausq sigsq phi")
    print(betas)
    print(c(tausq,sigsq,phi))
    print(sort(my.eigen$values))
    stop("here")
  }
  
  inv.evals <- 1/sqrt( my.eigen$values )
  term1 <- y.minus.mu %*% my.eigen$vectors %*% diag(inv.evals)
  term1 <- sum( term1^2 )
  
  #term1 <- y.minus.mu %*% solve(Sigma) %*% y.minus.mu
  
  LogL <- -n*0.5*log(2*pi) - 0.5*term1
  return( -2*LogL )
}


# now calculate DIC, by first reading our output file:
my.post <- read.table(fname,header=TRUE)
print(min( my.post$sigmasq ))
print(min( my.post$tausq ))
print(min( my.post$phi ))

# y-values are my.geodata$lgcatch

# need matrix of distances between obs. locations
distmat <- dist( cbind( my.geodata$coords[,1],my.geodata$coords[,2] ) )
distmat <- as.matrix( distmat, nrow = length( my.geodata$data ) )

my.mean.beta0 <- mean( my.post$beta0 )
my.mean.beta1 <- mean( my.post$beta1 )
my.mean.beta2 <- mean( my.post$beta2 )

my.mean.tausq <- mean( my.post$tausq )
my.mean.sigmasq <- mean( my.post$sigmasq )
my.mean.phi <- mean( my.post$phi )

D.theta.bar <- neg2logL.quadratic.trend( 
  c( my.mean.beta0, my.mean.beta1, my.mean.beta2),
  my.mean.tausq, my.mean.sigmasq,
  my.mean.phi,  my.geodata$data, 
  c.lons, c.lats )
D.theta.bar

# In order to calculate D.bar, we need to calculate
#  -2*log(L) for each MCMC iteration, then average
# these values to get D.bar:
neg2LogL.buf <- rep(NA,nrow(my.post))
for( i in 1:nrow(my.post) ) {
  neg2LogL.buf[i] <-
    neg2logL.quadratic.trend(
      c(my.post$beta0[i],my.post$beta1[i],
        my.post$beta2[i]),
      my.post$tausq[i], my.post$sigmasq[i],
      my.post$phi[i], my.geodata$data,
      c.lons,c.lats )
}
D.bar <- mean(neg2LogL.buf)
print(paste("D.bar = ",D.bar))

pD <- D.bar - D.theta.bar
print(paste("pD = ",pD))

DIC <- pD + D.bar
print(paste("DIC = ",DIC))

print("The end.")
