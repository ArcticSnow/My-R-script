# Script to estimate semivariogram of a given surface
# August 1, 2013, Simon Filhol

# write in function form if possible!!!

library(geoR)

# I. transform the surface into a geodata object
as.geodata()

# II. Calculate variogram

variog4(data,trend="",estimator.type="modulus",direction=c(0,pi/4,pi/2,3*pi/4),tolerance=pi/8) # robust, 4 direction

# III. Plot empirical semi variograms
plot()

# IV. Checksignificance of empirical semivariogram by randomizing data
# need to find excercise with code to do this!!!!!!! spatial statistics class








