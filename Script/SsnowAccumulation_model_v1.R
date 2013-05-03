#*************************************************************
#           SNOW ACCUMULATION MODELLING, probabilistic apporach
#*************************************************************
# by Simon Filhol, May 1st 2013


# Ground profile
x <- seq(0,40,.05)
zorig <- sin(x)
Grain.size <- .1
step=2
Repose <- 35  # in degree
wind=0  #in degree from vertical
noise <- 5 # in degree
z <- zorig
nIter=25000
nFlake=10

Accumulate(nIter=20000,x,z,wind,noise,Repose,Grain.size)

# TO DO LIST
# - Also adapt code to have multiple flake falling at same time (between 5-100)
# - Add function to save a z-profile every nIter/10 or so
# - try writing a function that adapt jump step according to incident angle of the flake (find trajectory equation)
# - try to put numbers with proper scale
# - see if can incorporate compaction of snow pack as function of overburden
# - create a square function close to the experiment one
# - try to think of how incoporating a Stickyness parameter function of a given temperature profile




Accumulate <- function(nIter,x,z,wind,noise,Repose,Grain.size,nFlake){
  motion=1
  zorig <- z
  for(i in 1:nIter){
    
    # Create snowflake
    reflake <- 1
    while(reflake == 1){
      flake <- round(runif(nFlake,min=min(x),max=max(x))*2,1)/2
      ind <- rep(0,times=nFlake)
      for(i in 1:nFlake){ind[i] <- which(abs(x-flake[i])<1e-3)}
      if(sum(ind==length(x))>=1 || sum(ind==1)>=1)
      {reflake <- 1}else{reflake <- 0}
    } 
    while(motion!="stopped"){
      
      # calculate slope angle from profile geometry and probability of staying
      alpha=atan(diff(z)/diff(x))*180/pi
      my.angle <- alpha-Incid(wind,noise)
      Px <- Prob.stay(my.angle[ind],Repose)
      
      # Decision rule to know if snow flake stays or note
      if(Px>rnorm(1,mean=.5,sd=.5/1.96)){
        for(i in 1:nFlake){
        z[ind[i]] <- z[ind[i]]+round(rnorm(1,mean=Grain.size,sd=Grain.size/(2*1.96)),4)
        }
        motion <- "stopped"
        # Smooth new profile
        z <- predict(smooth.spline(x,z,spar=.01))$y
      }else{
        
        # Adapt from here to multiple snowflake, my.angle[ind] is now a vector
        if(my.angle[ind]>0){
          if(flake-step<min(x)){
            motion <- "stopped"
          }else{
            flake <- flake-step}
        }
        else{
          if(flake+step>max(x)){
            motion <- "stopped"
          }else{
            flake <- flake+step
          }
        }
        
      }
    }
    motion <- 1
  }
  
  plot(x,zorig,type="l",ylim=c(min(zorig),max(z)))
  lines(x,z)
  
  return(z)
}

#***********************************************
#FUNCTIONS ====
# Modelling snow accumulation - Function

# P(stay on existing surface)
require(pracma)
Prob.stay <- function(alpha,Repose){
  if(alpha<(-Repose) | alpha>Repose){
    Prob.stay <- 0}
  else{
    p <- polyfit(c(-Repose,0,Repose),c(0,.8,0),2)
    Prob.stay <- polyval(p,alpha)
  }
  return(Prob.stay)
}

# Distribution incident snow flake fall
Incid <- function(wind,noise){
  angle <- rnorm(1,mean=(wind),sd=noise/1.96)
  return(angle)
}
