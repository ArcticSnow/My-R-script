#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#      SNOW Processes models
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Simon Filhol, 25 January 2013


# Fresh Snow Density as function of air Temperature
# Equation from Hedstrom and Pomeroy 1998, also in the book, Snow Ecology, p49
# This equation holds for fresh dry snow (boreal type)

Fresh.Snow.Density <- function(Ta){
  if(Ta>-5){stop("Only for Dry snow, does not hold for air temperature higher than -5°C")}  
      # the -5°c was chosen arbitrary as no clear line is drawn between dry 
      # and wet snow unless by the presence of liquid
  Rho.s  <- 67.9+51.25*exp(Ta/2.59)  
}
