# Function to load and plot Subnivean probe tables and example.
# by Simon Filhol, May 14th, 2013

LoadSubnivean <- function(Probe.offset,file){
  # Function to load Subnivean probe data into R
  # - Probe offset is the length of the blue probe offset
  # - file is the name of the .csv file. IF not given, the function will open a Window to search the file
  ifelse(missing(file),{my.file <- file.choose()},{my.file <- file})
  raw.data <- read.csv( my.file, quote="\"",header=TRUE)
  depth <- raw.data$depthV*0.6733+7.829-Probe.offset
  my.data <- raw.data
  
  # Substract depth difference to the ground
  hole.nbs <- max(raw.data$recordLocation)
  for(i in 1:hole.nbs){
    depth[raw.data$recordLocation==i] <- depth[raw.data$recordLocation==i]-min(depth[raw.data$recordLocation==i])
  }
  depth <- depth
  my.data$depth.cm <- depth
  return(my.data)
}

plot.profile <- function(my.data,profile.nbs){
  # function that plot profiles selected to be plotted
  # - my.data must be a dataframe from the function LoadSubnivean.r
  # - profile.nbs is a vector indicating which profiles to plot
  
  require(ggplot2)
  if(length(profile.nbs)==1){
    my.subset <- subset(my.data,recordLocation==profile.nbs)
    ggplot(my.subset,aes(IRV,depth.cm,colour=factor(recordLocation)))+geom_point()
    
  }else{
    my.subset <- subset(my.data,recordLocation==profile.nbs[1])
    for(i in 2:length(profile.nbs)){
      my.subset <- rbind(my.subset,subset(my.data,recordLocation==profile.nbs[i]))
    }
    
    ggplot(my.subset,aes(IRV,depth.cm,colour=factor(recordLocation)))+geom_point()+facet_grid(. ~recordLocation)
  }
}

##### Script to test

Probe.offset <- 50  #cm
my.data <- LoadSubnivean(50)
plot.profile(my.data,c(1,3,5,6))

