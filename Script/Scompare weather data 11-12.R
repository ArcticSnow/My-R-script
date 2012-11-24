#============================================================================
#    Script to Compare weather data between LiDAR site and snownet site
#============================================================================
# By Simon Filhol, 8th Nov 2012.


# Function to load:
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
library("ggplot2")
library("splines")


#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Import data from SnowNet Site, plot them out:
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# set working directory. WARNING, refers to an external Hardrive, SNOWBLUE !!!!!!
setwd("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Sonic_sounder/R_process")
# Read data from text file
data <- read.table("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Sonic_sounder/R_process/GC_snownet_table1.txt",header=TRUE,sep=",")

# extract data in numeric form
Net.time <- strptime(as.character(data[,1]),tz="",format="%Y-%m-%d %H:%M:%S")
Net.Record <- as.numeric(data[,2])
Net.Wind.speed <- as.numeric(data[,7])
Net.Wind.dir <- as.numeric(data[,8])
Net.RH <- as.numeric(data[,11])
Net.Temp <- as.numeric(data[,12])
Net.SD <- as.numeric(data[,13])

rm(data)

qplot(Net.time[1:21541], Net.SD[1:21541])
qplot(Net.time,Net.Temp,geom = c("smooth"),
      method = "lm", formula = y ~ ns(x, 100))
qplot(Net.time,Net.RH,geom = c("smooth"),
      method = "lm", formula = y ~ ns(x, 100))
qplot(Net.time,Net.Wind.speed,geom = c("smooth", "point"),
      method = "lm", formula = y ~ ns(x, 50))

#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#    IMPORT GC LiDAR SITE WEATHER DATA
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

data <- read.table("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Sonic_sounder/R_process/6may2012Table5min_for_R.txt",header=TRUE,sep=",")

# extract data in numeric form
Lid.Timestamp <- as.character(data[,1])
# Convert character into time object.
Lid.time <- strptime(Lid.Timestamp,tz="",format="%Y-%m-%d %H:%M:%S")
Lid.Record <- as.numeric(data[,2])

Lid.Wind.high <- list(time=strptime(as.character(data[,16]),tz="",format="%Y-%m-%d %H:%M:%S"), speed.max=as.numeric(data[,15]),time.min= strptime(as.character(data[,18]),tz="",format="%Y-%m-%d %H:%M:%S"),speed.min=as.numeric(data[,17]))
Lid.Wind.low <- list(time=strptime(as.character(data[,24]),tz="",format="%Y-%m-%d %H:%M:%S"), speed.max=as.numeric(data[,23]),time.min= strptime(as.character(data[,26]),tz="",format="%Y-%m-%d %H:%M:%S"),speed.min=as.numeric(data[,25]))

Lid.RH <- as.numeric(data[,13])
Lid.Temp <- as.numeric(data[,8])
Lid.SD <- as.numeric(data[,42])
Lid.GrTemp <- as.numeric(data[,44])
Lid.Pressure <- as.numeric(data[,14])

rm(data,Lid.Timestamp)

#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#    Comparison of data between the two met station
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

# truncate data of SnowNet data to the same period as LiDAR weather station
Net.ind <- which(Net.time>= min(Lid.time) & Net.time<= max(Lid.time))
Net.time <- Net.time[Net.ind]
Net.Record <- Net.Record[Net.ind]
Net.Wind.speed <- Net.Wind.speed[Net.ind]
Net.Wind.dir <- Net.Wind.dir[Net.ind]
Net.RH <- Net.RH[Net.ind]
Net.Temp <- Net.Temp[Net.ind]
Net.SD <-Net.SD[Net.ind]
rm(Net.ind)

# Delete outliers in LiDAR Snow depth:
Lid.SD<- ifelse(Lid.SD<1 & Lid.SD >(-0.2),Lid.SD,NA)

#Select max wind
require(xts)
Net.wind.xts <- as.xts(Net.Wind.speed,order.by=Net.time)
Net.max.wind.xts <- apply.daily(Net.wind.xts,max)  # pick up maximum daily value
Lid.wind.xts <- as.xts(Lid.Wind.high$speed.max,order.by=Lid.Wind.high$time) # only take data from higher sensor
Lid.max.wind.xts <- apply.daily(Lid.wind.xts,max)  # pick up maximum daily value


# find common time stamp between the two met station
Net.ind <- match(Lid.time,Net.time,nomatch=0)
Net.ind <- Net.ind[Net.ind!=0]
Net.time.com <- Net.time[Net.ind]
Lid.ind <- match(Net.time.com,Lid.time,nomatch=0)
Lid.ind=Lid.ind[Lid.ind!=0]
Lid.time.com <- Lid.time[Lid.ind]


S.D <- data.frame(
  Site = as.factor(c( rep( "SnowNet",length.out=length(Net.time[!is.na(Net.SD)]) ),
                      rep("LiDAR",length.out=length(Lid.time[!is.na(Lid.SD)]) )  )) ,
  time = c(Net.time[!is.na(Net.SD)],Lid.time[!is.na(Lid.SD)]),
  val=c(Net.SD[!is.na(Net.SD)],Lid.SD[!is.na(Lid.SD)])
  )
 
Temp <- data.frame(
  Site = as.factor(c( rep( "SnowNet",length.out=length(Net.time[!is.na(Net.Temp)]) ),
                      rep("LiDAR",length.out=length(Lid.time[!is.na(Lid.Temp)]) )  )) ,
  time = c(Net.time[!is.na(Net.Temp)],Lid.time[!is.na(Lid.Temp)]),
  val=c(Net.Temp[!is.na(Net.Temp)],Lid.Temp[!is.na(Lid.Temp)])
)

Wind <- data.frame(
  Site = as.factor(c( rep( "SnowNet",length.out=length(Net.max.wind.xts) ),
                      rep("LiDAR",length.out=length(Lid.max.wind.xts) ) ) ) ,
  time=c(index(Net.max.wind.xts),index(Lid.max.wind.xts)),
  val=c(as.vector(Net.max.wind.xts),as.vector(Lid.max.wind.xts))
)


p01 <- qplot(time,val,data=S.D,colour=Site,geom="line",
            xlab="Time",ylab="Snow Depth (m)")
p02 <- qplot(time,val,data=Temp,colour=Site,geom="line",
             xlab="Time",ylab="Temperature (°C)")
p03 <- qplot(time,val,data=Wind,colour=Site,geom="line",
             xlab="Time",ylab="Wind speed (m/s)",main="Daily Maximum Wind Speed")
multiplot(p01,p02,p03,cols=1)

p1 <- qplot(Lid.Temp[(Lid.ind)],Net.Temp[(Net.ind)],geom=c("point","smooth"),method=lm,
            xlab='Temperature at LiDAR site (degC)',ylab='Temperature at SnowNet site (degC)',main="LiDAR and SnowNet Site Weather Data Comparison")
p2 <- qplot(Lid.SD[(Lid.ind)],Net.SD[(Net.ind)],geom=c("point","smooth"),method=lm,
            xlab='Snow Depth at LiDAR site (m)',ylab='Snow Depth at SnowNet site (m)') 
p3 <- qplot(Lid.RH[(Lid.ind)],Net.RH[(Net.ind)],geom=c("point","smooth"),method=lm,
                  xlab='RH at LiDAR site (%)',ylab='RH at SnowNet site (%)')
multiplot(p1,p2,p3,cols=1)

p4 <-  qplot(Lid.time,Lid.SD,geom=c("point"),
            xlab='Time',ylab='Snow Depth (cm)',main="At the LiDAR Site") 
p5 <- qplot(Net.time,Net.SD,geom=c("point"),
      xlab='Time',ylab='Snow Depth (cm)', main="At the SnowNet site") 
multiplot(p4,p5,cols=1)

#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# script to process Sounic sounder data from winter 2011-2012
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Read data from text file
data <- read.table("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Sonic_sounder/R_process/Ready_for_R.txt",header=TRUE,sep=",")

# extract data in numeric form
Max.Timestamp <- as.character(data[,1])
Max.Record <- as.numeric(data[,2])
Max.Batt.volt <- as.numeric(data[,3])
Max.Ptemp <- as.numeric(data[,4])
Max.Temp.107 <- as.numeric(data[,5])
Max.signal <- matrix(NA,nrow=length(Max.Ptemp),ncol=8)
Max.power <- matrix(NA,nrow=length(Max.Ptemp),ncol=8)
for(k in 0:7){
  Max.signal[,k+1] <- as.numeric(data[,6+k])
  Max.power[,k+1] <- as.numeric(data[,14+k])
}

# Convert character into time object.
Max.time <- strptime(Max.Timestamp,tz="",format="%Y-%m-%d %H:%M:%S")

#==========================================================================
# Convert time to distance
#==========================================================================

#Constant for Travel of Sound correction
R <- 8.314         # Absolute gas constant  J/(mol.K)
Gamma <- 1.4       # Adiabatic constant
M <- 0.0289645     # Molecular mass  kg/mol
Max.speed  <-  (R*Gamma*(Max.Ptemp+273.15)/M)^0.5  # speed of sound wave in air at temperature Ptemp

#Table calibration constant for each sensor, see Igor Pro experiment
#"calibration.pxp"

a <- c(8.36646e-05,  3.81954e-05, 0.000114945, 2.863e-05, -3.55322e-05, -6.08513e-05, -0.000196238, -0.000168229)
b <- c(1.33512e-05, 1.34752e-05, 1.33613e-05, 1.34665e-05, 1.37847e-05, 1.37732e-05, 1.80333e-05, 1.8148e-05)

# Calulation of time of trave of sound in air acording to Temperature.
Max.Distance  <-  matrix(0,nrow=length(Max.Record),ncol=8)
Max.snowdepth <- matrix(0,nrow=length(Max.Record),ncol=8)
for (i in 1:8){
  Max.Tov  <-  Max.signal[,i]*b[i]+a[i]
  for(k in 1:length(Max.Tov)){
    if (Max.Tov[k]<0.002 & !is.na(Max.Tov[k])){
      Max.Tov[k] <- NA
    } 
    if (Max.Tov[k]>0.01 & !is.na(Max.Tov[k])) {
      Max.Tov[k] <- NA
    }
  }
  Max.Distance[,i]  <-  Max.Tov/2 * Max.speed
}

# Height of each maxbotix
Max.heigth <- c(mean(Max.Distance[1:10000,1],na.rm=TRUE),mean(Max.Distance[1:10000,2],na.rm=TRUE),
                     mean(Max.Distance[1:10000,3],na.rm=TRUE),mean(Max.Distance[1:10000,4],na.rm=TRUE),
                     mean(Max.Distance[1:10000,5],na.rm=TRUE),mean(Max.Distance[1:5000,6],na.rm=TRUE),
                     mean(Max.Distance[1:10000,7],na.rm=TRUE),mean(Max.Distance[1:10000,8],na.rm=TRUE))

for (i in 1:8){
  Max.snowdepth[,i] <- - Max.Distance[,i]+Max.heigth[i]
}


#==========================================================================
# Combine all snow depth data
#==========================================================================

S.D.all <- data.frame(
  Site = as.factor(c( rep( "SnowNet",length.out=length(Net.time[!is.na(Net.SD)])),
                      rep("LiDAR",length.out=length(Lid.time[!is.na(Lid.SD)])),
                      rep("Maxbotix 1",length.out=length(Max.time)),
                      rep("Maxbotix 3",length.out=length(Max.time)),
                      rep("Maxbotix 4",length.out=length(Max.time)),
                      rep("Maxbotix 5",length.out=length(Max.time)),
                      rep("Maxbotix 6",length.out=length(Max.time)),
                      rep("Maxbotix 7",length.out=length(Max.time)))) ,
  time = c(Net.time[!is.na(Net.SD)],Lid.time[!is.na(Lid.SD)],
           Max.time,
           Max.time,
           Max.time,
           Max.time,
           Max.time,
           Max.time
           ),
  val=c(Net.SD[!is.na(Net.SD)],
        Lid.SD[!is.na(Lid.SD)],
        Max.snowdepth[,1],
        Max.snowdepth[,3],
        Max.snowdepth[,4],
        Max.snowdepth[,5],
        Max.snowdepth[,6],
        Max.snowdepth[,7])
)

qplot(time,val,data=S.D.all,colour=Site,geom="smooth",method = "lm", formula = y ~ ns(x, 100),
             xlab="Time",ylab="Snow Depth (m)")


all.max=c(max(Max.snowdepth[,1],na.rm=T),
          max(Max.snowdepth[,3],na.rm=T),
          max(Max.snowdepth[,4],na.rm=T),
          max(Max.snowdepth[,5],na.rm=T),
          max(Max.snowdepth[,6],na.rm=T),
          max(Max.snowdepth[,7],na.rm=T),
          max(Net.SD,na.rm=T),
          max(Lid.SD,na.rm=T))
qplot(1,all.max,geom=c("boxplot","point"),xlab="Boxplot",ylab="Maximum Snowdepth (m)")

#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Add loading-unloading / snowfall event to graph
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Read data from text file
sfl.data <- read.table("/Users/simonfilhol/PhD/Snow/Glenn_Greek/Met Station/snow_fall_load_event_1112.csv",header=TRUE,sep=",")

sfl.Timestamp <- as.character(sfl.data$date)
sfl.loading <- as.numeric(sfl.data$snow_load)
sfl.fall <- as.numeric(sfl.data$snow_fall)
# Convert character into time object.
sfl.time <- strptime(sfl.Timestamp,tz="",format="%m/%d/%y")

sfl <- data.frame(
  time=sfl.time,
  Loading=sfl.loading,
  Fall=sfl.fall
  )

h <- ggplot()+
  geom_line(aes(x=Temp$time[Temp$Site=="SnowNet"],y=Temp$val[Temp$Site=="SnowNet"]),
            colour=ifelse(Temp$val[Temp$Site=="SnowNet"]>=0,"red","blue"))+
  geom_point(aes(x=sfl$time,y=sfl$Fall/2+max(Temp$val+2,na.rm=T)),shape='*',size=(sfl$Fall)*7,
             colour=ifelse(sfl$Fall>=0,"blue","red"))+
  ylab("Temperature (°C)")+
  xlab("Time")
 
w <- ggplot()+
  geom_line(aes(x=Wind$time[Wind$Site=="LiDAR"],y=Wind$val[Wind$Site=="LiDAR"]))+
  geom_point(aes(x=sfl$time,y=-sfl$Loading/2+max(Wind$val+2,na.rm=T)),shape=17,size=(sfl$Loading*1.1))+
  ylab("Wind Speed (m/s)")+
  xlab("Time")

p01 <- qplot(time,val*100,data=S.D,colour=Site,geom="line",
             xlab="Time",ylab="Snow Depth (cm)")+
  theme(legend.justification=c(1,0), legend.position=c(1,0.5))

multiplot(h,p01,w,cols=1)

diffe <- diff(S.D$val[S.D$Site=="SnowNet"],lag=96)
diffe <- c(rep(0,times=length(S.D$time[S.D$Site=="SnowNet"])-length(diffe)),diffe)
Diff <- data.frame(
  val=diffe,
  time=S.D$time[S.D$Site=="SnowNet"])
d <- ggplot()+
  geom_line(aes(x=Diff$time,y=Diff$val) ,colour=ifelse(Diff$val>0,"blue","red"))+
  geom_point(aes(x=sfl$time,y=-sfl$Fall/10-0.1,na.rm=T),
shape='*',
size=(sfl$Fall)*7,
colour=ifelse(sfl$Fall>=0,"blue","red"))
multiplot(d,p01,h)
