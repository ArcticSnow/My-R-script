# analysis of long snowpit for subnivean space estimation

setwd("F:/Phd/Research/Snownet Data/GlennCreek/Winter 11_12/Snowpit/Subnivean experiment")

pit.1 <- read.table("F:/Phd/Research/Snownet Data/GlennCreek/Winter 11_12/Snowpit/Subnivean experiment/pit 1.txt",header=TRUE)
pit.2 <- read.table("F:/Phd/Research/Snownet Data/GlennCreek/Winter 11_12/Snowpit/Subnivean experiment/pit 2.txt",header=TRUE)
pit.3 <- read.table("F:/Phd/Research/Snownet Data/GlennCreek/Winter 11_12/Snowpit/Subnivean experiment/pit 3.txt",header=TRUE)

plot(pit.1[,1],pit.1[,5])
library(pracma)

snow.1 <- cbind(pit.1[,1],pit.1[,4])
ground.1 <- cbind(pit.1[,1],pit.1[,5])
snow.area.1 <- trapz(snow.1[,1],snow.1[,2])
total.area.1 <- trapz(ground.1[,1],ground.1[,2])
void.1 <- total.area.1-snow.area.1
percent.1 <- void.1/total.area.1
print(void.1)



snow.2 <- cbind(pit.2[,1],pit.2[,4])
ground.2 <- cbind(pit.2[,1],pit.2[,5])
snow.area.2 <- trapz(snow.2[,1],snow.2[,2])
total.area.2 <- trapz(ground.2[,1],ground.2[,2])
void.2 <- total.area.2-snow.area.2
percent.2 <- void.2/total.area.2
print(c(void.2,percent.2))



snow.3 <- cbind(pit.3[,1],pit.3[,4])
ground.3 <- cbind(pit.3[,1],pit.3[,5])
snow.area.3 <- trapz(snow.3[,1],snow.3[,2])
total.area.3 <- trapz(ground.3[,1],ground.3[,2])
void.3 <- total.area.3-snow.area.3
percent.3 <- void.3/total.area.3
print(c(void.3,percent.3))

layout(c(1,2,3),heights=c(1,1,1))
plot(pit.1[,1],pit.1[,4],type="n",ylim=rev(c(0,70)),main="Snowpit 1",xlab="",ylab="Snow depth(cm)")

lines(pit.1[,1],pit.1[,2],col="grey")
polygon(c(pit.1[,1],rev(pit.1[,1])),c(pit.1[,2],rev(pit.1[,4])),col="#D4d4d4")
lines(pit.1[,1],pit.1[,4],col="grey")
lines(pit.1[,1],pit.1[,5],col="grey")
polygon(c(pit.1[,1],rev(pit.1[,1])),c(pit.1[,4],rev(pit.1[,5])),col="#bcbcbc")
lines(pit.1[,1],pit.1[,3],lty="dotted")

plot(pit.2[,1],pit.2[,4],type="n",ylim=rev(c(0,70)),main="Snowpit 2",xlab="",ylab="Snow depth(cm)")
lines(pit.2[,1],pit.2[,2],col="grey")
polygon(c(pit.2[,1],rev(pit.2[,1])),c(pit.2[,2],rev(pit.2[,4])),col="#D4d4d4")
lines(pit.2[,1],pit.2[,4],col="grey")
lines(pit.2[,1],pit.2[,5],col="grey")
polygon(c(pit.2[,1],rev(pit.2[,1])),c(pit.2[,4],rev(pit.2[,5])),col="#bcbcbc")
lines(pit.2[,1],pit.2[,3],lty="dotted")

plot(pit.3[,1],pit.3[,4],type="n",ylim=rev(c(0,70)),main="Snowpit 3",ylab="Snow depth(cm)",xlab="Distance (cm)")
lines(pit.3[,1],pit.3[,2],col="grey")
polygon(c(pit.3[,1],rev(pit.3[,1])),c(pit.3[,2],rev(pit.3[,4])),col="#D4d4d4")
lines(pit.3[,1],pit.3[,4],col="grey")
lines(pit.3[,1],pit.3[,5],col="grey")
polygon(c(pit.3[,1],rev(pit.3[,1])),c(pit.3[,4],rev(pit.3[,5])),col="#bcbcbc")
lines(pit.3[,1],pit.3[,3],lty="dotted")







