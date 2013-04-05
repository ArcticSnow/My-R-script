
# by Simon Filhol, April 4th,
# based on method from wikipedia (http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system)
# given by C. F. F. Karney (2011), Transverse Mercator with an accuracy of a few nanometers, J. Geodesy 85(8), 475–485 

latlong.2.UTM <- function(latlong,hemisphere){
  # list of constant
  a <- 6378.137     #earth radius in km
  f <- 1/298.257223563   # flattening
  ko <- 0.9996
  Eo <- 500
  if(hemispher=="south"){No=10000}else{No=0}
  
  n <- f/(2-f)
  A <- a/(A+n)*(1+n^2/4+n^4/64)
  Alpha1 <- 1*n/2-2*n^2/3+5*n^3/16
  Alpha2 <- 13/48*n^2-3/5*n^3
  Alpha3 <- 61/240*n^3
  Beta1 <- 1/2*n-2/3*n^2+37/96*n^3
  Beta2 <- 1/48*n^2+1/15*n^3
  Beta3 <- 17/480*n^3
  Gamma1 <- 2*n-2/3*n^2-2*n^3
  Gamma2 <- 7/3*n^2-8/5*n^3
  Gamma3 <- 56/15*n^3
  
  t <- sinh()
  
  
}



UTM.2.latlong <- function(NE,zone,hemisphere){}