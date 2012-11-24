# script to process Sounic sounder data from winter 2011-2012

# set working directory. WARNING, refers to an external Hardrive, SNOWBLUE !!!!!!
setwd("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Sonic_sounder/R_process")
# Read data from text file
data <- read.table("/Volumes/SNOW BLUE/PhD/Research/SnowNet/Glenn Creek/Sonic_sounder/R_process/Ready_for_R.txt",header=TRUE,sep=",")

# extract data in numeric form
Timestamp <- as.character(data[,1])
Record <- as.numeric(data[,2])
Batt.volt <- as.numeric(data[,3])
Ptemp <- as.numeric(data[,4])
Temp.107 <- as.numeric(data[,5])
my.signal <- matrix(NA,nrow=length(Ptemp),ncol=8)
my.power <- matrix(NA,nrow=length(Ptemp),ncol=8)
for(k in 0:7){
my.signal[,k+1] <- as.numeric(data[,6+k])
my.power[,k+1] <- as.numeric(data[,14+k])
}

# Convert character into time object.
my.time <- strptime(Timestamp,tz="",format="%Y-%m-%d %H:%M:%S")

library("ggplot2")

#==========================================================================
# Convert time to distance

#Constant for Travel of Sound correction
R <- 8.314         # Absolute gas constant  J/(mol.K)
Gamma <- 1.4       # Adiabatic constant
M <- 0.0289645     # Molecular mass  kg/mol
my.speed  <-  (R*Gamma*(Ptemp+273.15)/M)^0.5  # speed of sound wave in air at temperature Ptemp


#Table calibration constant for each sensor, see Igor Pro experiment
#"calibration.pxp"

a <- c(8.36646e-05,  3.81954e-05, 0.000114945, 2.863e-05, -3.55322e-05, -6.08513e-05, -0.000196238, -0.000168229)
b <- c(1.33512e-05, 1.34752e-05, 1.33613e-05, 1.34665e-05, 1.37847e-05, 1.37732e-05, 1.80333e-05, 1.8148e-05)

# Calulation of time of trave of sound in air acording to Temperature.
Distance  <-  matrix(0,nrow=length(Record),ncol=8)
my.snowdepth <- matrix(0,nrow=length(Record),ncol=8)
for (i in 1:8){
Tov  <-  my.signal[,i]*b[i]+a[i]
for(k in 1:length(Tov)){
  if (Tov[k]<0.002 & !is.na(Tov[k])){
    Tov[k] <- NA
  } 
  if (Tov[k]>0.01 & !is.na(Tov[k])) {
    Tov[k] <- NA
  }
}
Distance[,i]  <-  Tov/2 * my.speed
}

# Height of each maxbotix
maxbotix.heigth <- c(mean(Distance[1:10000,1],na.rm=TRUE),mean(Distance[1:10000,2],na.rm=TRUE),
                     mean(Distance[1:10000,3],na.rm=TRUE),mean(Distance[1:10000,4],na.rm=TRUE),
                     mean(Distance[1:10000,5],na.rm=TRUE),mean(Distance[1:5000,6],na.rm=TRUE),
                     mean(Distance[1:10000,7],na.rm=TRUE),mean(Distance[1:10000,8],na.rm=TRUE))

for (i in 1:8){
  my.snowdepth[,i] <- - Distance[,i]+maxbotix.heigth[i]
}

library("splines")

qplot(my.time, my.snowdepth[,6], geom = c("smooth"),
      method = "lm", formula = y ~ ns(x, 100))


