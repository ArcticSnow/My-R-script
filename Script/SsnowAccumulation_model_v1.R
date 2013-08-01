#*************************************************************
#           SNOW ACCUMULATION MODELLING, probabilistic apporach
#*************************************************************
# by Simon Filhol, May 1st 2013

library(fBasics)
# Ground profile
x <- seq(0,2,.001)

# Working here to get a multiple step function as topographic profile
zorig <- x*0
for(i in seq(min(x),max(x),.5)){
for(j in 1:length(x)){
  if(x[j] <i+(x[2]-x[1] /2) & x[j]>i){
  zorig[j] <- .25
}
}
}

Grain.size <- .001
step=.5
Repose <- 35  # in degree
wind=0  #in degree from vertical
noise <- 5 # in degree
z <- zorig
nIter=20000
nFlake=10
nProfile=10

a <- Accumulate(nIter,x,z,wind,noise,Repose,Grain.size,nFlake,nProfile)

plot(x,a$my.topo,type='l',ylim=c(min(a$my.topo),max(a$my.profile[nProfile,])))
for(i in 2:nProfile){
  lines(x,a$my.profile[i,])
}



# TO DO LIST
# - try writing a function that adapt jump step according to incident angle of the flake (find trajectory equation)
# - try to put numbers with proper scale
# - see if can incorporate compaction of snow pack as function of overburden
# - create a square function close to the experiment one
# - try to think of how incoporating a Stickyness parameter function of a given temperature profile




Accumulate <- function(nIter,x,z,wind,noise,Repose,Grain.size,nFlake,nProfile){
  tic()
  if(nFlake>length(x)){stop("Nflake cannot be more than length(x)")}
  motion=1
  zorig <- z
  profiles <- matrix(NaN,nrow=nProfile,ncol=length(z))
  sampling <- round(seq(1,nIter,length.out=nProfile))
  k=1
  for(i in 1:nIter){
    
    # Create snowflake
    reflake <- 1
    while(reflake == 1){
      flake <- round(runif(nFlake,min=min(x),max=max(x))*2,1)/2
      ind <- rep(0,times=nFlake)
      for(n in 1:nFlake){ind[n] <- which(abs(x-flake[n])<1e-5)}
      if(sum(ind==length(x))>=1 || sum(ind==1)>=1)
      {reflake <- 1}else{reflake <- 0}
    } 
    while(motion!="stopped"){
      # calculate slope angle from profile geometry and probability of staying
      alpha=atan(diff(z)/diff(x))*180/pi
      my.angle <- alpha-Incid(wind,noise)
      Px <- lapply(my.angle[ind],Prob.stay,Repose=Repose)
#       Px <- rep(0,times=length(ind))
#       for(m in 1:length(ind)){  
#       Px[i] <- Prob.stay(my.angle[ind[i]],Repose)
#       }
      # Decision rule to know if snowflake stays or note
      for(j in 1:nFlake){
        if(Px[j]>runif(1,min=0,max=1)){
          z[ind[j]] <- z[ind[j]]+round(rnorm(1,mean=Grain.size,sd=Grain.size/(2*1.96)),4)
          motion <- "stopped"
        }else{
          # Adapt from here to multiple snowflake, my.angle[ind] is now a vector
          if(my.angle[ind[j]]>0){
            if(flake[j]-step<min(x)){
              motion <- "stopped"
            }else{
              flake[j] <- flake[j]-step}
          }
          else{
            if(flake[j]+step>max(x)){
              motion <- "stopped"
            }else{
              flake[j] <- flake[j]+step
            }
          }
        }
      }
      # Smooth new profile
      #z <- predict(smooth.spline(x,z,spar=.01))$y
    }
    motion <- 1
    if(i==sampling[k]){
      profiles[k,] <- z
      k=k+1
    }
    
  }
  toc()
  my.return=list(my.topo=zorig,
                 my.final.profile=z,
                 my.profiles=profiles)
  return(my.return)
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
