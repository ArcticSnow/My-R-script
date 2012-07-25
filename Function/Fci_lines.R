# Function to plot the linear regression and overlay the confidence intervals
# from : http://stackoverflow.com/questions/2687212/conditionally-colour-data-points-outside-of-confidence-bands-in-r

# Model= model from the function lm()
# conf= interval of interest 95%, 80%, 99%
# interval = "confidence" or "prediction"


ci.lines<-function(model,conf= .95 ,interval = "confidence"){
  x <- model[[12]][[2]]
  y <- model[[12]][[1]]
  xm<-mean(x)
  n<-length(x)
  ssx<- sum((x - mean(x))^2)
  s.t<- qt(1-(1-conf)/2,(n-2))
  xv<-seq(min(x),max(x),(max(x) - min(x))/100)
  yv<- coef(model)[1]+coef(model)[2]*xv
  
  se <- switch(interval,
               confidence = summary(model)[[6]] * sqrt(1/n+(xv-xm)^2/ssx),
               prediction = summary(model)[[6]] * sqrt(1+1/n+(xv-xm)^2/ssx)
               )
  
  ci<-s.t*se
  uyv<-yv+ci
  lyv<-yv-ci
  limits1 <- min(c(x,y))
  limits2 <- max(c(x,y))
  
  predictions <- predict(model, level = conf, interval = interval)
  
  insideCI <- predictions[,'lwr'] < y & y < predictions[,'upr']
  
  x_name <- rownames(attr(model[[11]],"factors"))[2]
  y_name <- rownames(attr(model[[11]],"factors"))[1]
  
  plot(x[insideCI],y[insideCI],
       pch=16,pty="s",xlim=c(limits1,limits2),ylim=c(limits1,limits2),
       xlab=x_name,
       ylab=y_name,
       main=paste("Graph of ", y_name, " vs ", x_name,sep=""))
  
  abline(model)
  
  points(x[!insideCI],y[!insideCI], pch = 16, col = 'red')
  
  lines(xv,uyv,lty=2,col=3)
  lines(xv,lyv,lty=2,col=3)
}
##################################################
# Example:

#   diseasesev<-c(1.9,3.1,3.3,4.8,5.3,6.1,6.4,7.6,9.8,12.4)
# # Predictor variable, (Centigrade)
# temperature<-c(2,1,5,5,20,20,23,10,30,25)
# 
# ## For convenience, the data may be formatted into a dataframe
# severity <- as.data.frame(cbind(diseasesev,temperature))
# 
# ## Fit a linear model for the data and summarize the output from function lm()
# severity.lm <- lm(diseasesev~temperature,data=severity)
# ci.lines(severity.lm, conf= .95 , interval = "confidence")
# ci.lines(severity.lm, conf= .85 , interval = "prediction")

