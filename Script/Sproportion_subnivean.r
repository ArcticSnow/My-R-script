# Simon Filhol
# 9 august 2012

# please refer also to file Subnivean_dynamic.xlsx

# these are data from the snowpit collected during winter 2011-2012 all across winter. they can be complemented 
# by the data of the 3 long snowpit (Sprocess_subnivean.r)


bot_8dec <- c(26,25,  11,19, 18,10,13, 18,10,0)
top_8dec <- c(35.5,39,27.5,32, 29, 20, 24, 25.5, 24,    22)

bot_15dec <- c(12, 10, 8, 13, 13, 13, 16,  20,  10, 0, 5)
top_15dec <- c(24,25, 27, 26,28,28,38,32,25,24,21.5)

bot_22dec <- c(11,16, 9, 16, 13,8,10, 3, 0,5, 5)
top_22dec <- c(26,27.5,27.5,28,  24.5,24, 31,31.5,28, 26,24.5)


bot_7fev <- c(14, 17,  0, 0, 13,0,5, 0,  0, 0)
top_7fev <- c(49, 51,40,46,59.5,55, 48.5,41, 40, 38)

bot_5mar <- c(9,4,19, 10,5,0, 0,10, 7)
top_5mar <- c(51, 36,56,58.5,57,50,50, 50,49)


bot_5mar <- c(9,4,19, 10,5,0, 0,10, 7)
top_5mar <- c(51, 36,56,58.5,57,50,50, 50,49)



spacing <- seq(from=0,to=(length(top_15dec)-1)*10,by=10)
air <- cbind(spacing,bot_15dec)

air <- rbind(air,c(max(spacing),0))
air <- rbind(c(0,0),air)

total <- cbind(spacing,top_15dec)

total <- rbind(total,c(max(spacing),0))
total <- rbind(c(0,0),total)
library(splancs)
plot(total,type="l")
lines(air)



areapl(air)
areapl(total)


air <- c(bot_8dec,bot_15dec,bot_22dec,bot_5mar)
top <- c(top_8dec,top_15dec,top_22dec,top_5mar)
snow <- top-bot

my.data <- data.frame(air,snow,top)
library("ggplot2")
library("MASS")
qplot(air, snow, data = my.data, geom = c("point", "smooth"),method = "lm")

