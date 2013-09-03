# Script to estimate semivariogram of a given surface
# August 1, 2013, Simon Filhol

# write in function form if possible!!!

library(geoR)
library(R.matlab)
ifelse({Sys.info()['sysname']=="Windows"},{
  # need to find source of F_shuffle(...).r on windows 
},{
  source("/Users/simonfilhol/github/local/My_R_script/Function/F_Shuffle_test_sp_model.r")
})

# I. transform the surface into a geodata object
filename <- file.choose()
data <- readMat(filename)

summary(data)

my.geodata <- as.geodata()
plot(my.geodata)

# II. Calculate variogram

my.robust.vario.directional <- variog4(my.geodata,trend="cte",estimator.type="modulus",direction=c(0,pi/4,pi/2,3*pi/4),tolerance=pi/8)
my.robust.vario <- variog(my.geodata,trend="cte",estimator.type="modulus")

# III. Plot empirical semi variograms
plot(my.robust.vario.directional,xlab="lag (m)",ylab="semivariance",main="Empirical directional semivariogram" )
plot(my.robust.vario,xlab="lag (m)",ylab="semivariance",main="Empirical semivariogram")

# IV. Check significance of empirical semivariogram by randomizing data
Test.sp.model(my.robust.vario,my.geodata,"cte","Random shuffle test, 100 simulations")

my.var.fit <- variofit(my.robust.vario,ini.cov.pars=c(0.005,0.025),cov.model="exponential",fix.nugget=FALSE,nugget=0.005,weights="equal",max.dist=1.5)
lines(my.var.fit)
my.tausq <- my.var.fit$nugget
my.sigsq <- my.var.fit$cov.pars[1]
my.phi <- my.var.fit$cov.pars[2]



