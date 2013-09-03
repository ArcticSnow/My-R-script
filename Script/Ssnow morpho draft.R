

# Create 2D matrix
n <- 500
my.mat <- matrix(0,ncol=round(n/(2*pi)),nrow=round(n/(1.5*pi)))

for(i in 1:round(n/(2*pi))){
  for(j in 1:round(n/(1.5*pi))){
  my.mat[j,i] <- sin(i/4)/2+sin(j*4/10)
}}

dev.off()
dev.off()
image(my.mat)

plot(my.mat[10,])
plot(my.mat[,10])

my.spect <- spectrum(my.mat[10,])
my.spect <- spectrum(my.mat[,10])

# From matrix to xyz [TO be FINISHED]
#matrix2xyz <- function


# transform from XYZ matrix to polar coordinate
xyz2polar <- function(xyz){
  trans <- c( (max(xyz[,1]-min(xyz[,1])))/2, (max(xyz[,2]-min(xyz[,2])))/2 )
  xyz[,1] <- xyz[,1]-trans[1]
  xyz[,2] <- xyz[,2]-trans[2]
  alpha <- atan(xyz[,2]/xyz[,1])
  r <- xyz[,2]/(sin(alpha))
  polar <- cbind(round(r,digits=3),round(alpha,digits=3),round(xyz[,3],digits=3))
  return(polar)
}

# transform from polar vector to xyz vector
polar2xyz <- function(polar){
  y <- sin(polar[,2])*polar[,1]
  x <- polar[,1]*cos(polar[,2])
  xyz <- cbind(x,y,polar[,3]) 
  return(xyz)
}

# random rotate function
randomRotate <- function(polar){
  rand.angle <- round(runif(1,min=0, max=2*pi),digits=1)
  polar[,2] <- polar[,2]+rand.angle 
  polar[polar[,2]>2*pi,2] <- round(polar[polar[,2]>2*pi,2]-2*pi,digits=3)
  my.list <- list(polar= polar,angle= rand.angle)
  return(my.list)
}

# Select randomly a line of point within a width of dx
LineSample <- function(xyz,dy){
  a <- min((xyz[,2]),na.rm=TRUE)
  b <- max((xyz[,2]),na.rm=TRUE)
   y.pick <- round(runif(1,min=a,max=b ),digits=3)
  print(paste('Y.pick=',y.pick))
   my.new.xyz <- xyz[xyz[,2]>y.pick-dy/2  & xyz[,2]<=y.pick+dy/2,] 
  print(my.new.xyz)
   my.line <- cbind(my.new.xyz[,1],my.new.xyz[,3])
   return(my.line)
}

# test line sampling
x <- round(runif(10000,min=0,max=500),digits=3)
y <- round(runif(10000,min=0,max=100),digits=3)
z <- sin(x)+cos(y)
my.xyz <- cbind(x,y,z)
my.polar <- xyz2polar(my.xyz)
Rotated.list <- randomRotate(my.polar)
to.sample <- polar2xyz(Rotated.list$polar)
my.sample <- LineSample(to.sample,dy=20)
plot(my.sample)
plot(y,z)






# 1. Pick up two point in the matrix randomly to draw a line in between
my.x <- runif(2,min=1,max=dim(my.mat)[1])
my.y <- runif(2,min=1,max=dim(my.mat)[2])
 
# 2. get data from pixels under line defined by the two points
# use algorithm from http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm to find which pixel is under the line

# variables
n.profile <- 20            # number of profile to run
my.angles <- zeros(n.profile)     # vector recording angles of the line

# firts home made trial:
# a.coef <- diff(my.y)/diff(my.x)
# b.coef <- my.x[1]*a.coef-my.y[1]
# 
# for(i in 1:max=dim(my.mat)[1]){
#   my.profile[j] <- mean(my.mat[i,my.mat-dx>=   & my.mat+dx=< ])
# }}



# resample the section line to have regularly spaced data to feed spectrum(). use either smoothing or interpolation
# for each profile record in a vector, teh angle of the line

my.angles <- atan(a.coef)*180/pi


# 3.  use spectrum()
need to make sure the 

for(i in 1:dim(my.values)[1]){
  spec <- sepctrum(my.valeus[i,])
  my.spec.image[i,] <- spec$spec
}

# 4. plotting









# 



