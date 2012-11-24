FitPlane <- function(XYZ){
# fitplane function adapted from Matlab to R:  
  
#    FITPLANE - solves coefficients of plane fitted to 3 or more points
#   
#    Usage:   B = fitplane(XYZ)
#   
#    Where:   XYZ - Npts*3 array of xyz coordinates to fit plane to.   
#                   If Npts is greater than 3 a least squares solution 
#                   is generated.
#   
#    Returns: Bs   - 4x1 array of plane coefficients in the form
#                   b(1)*X + b(2)*Y +b(3)*Z + b(4) = 0
#                   The magnitude of B is 1.
#                   To get gtound surface detrend, just estimate the 
#                   residuals which are equal to
#                   Residual = b(1)*X + b(2)*Y +b(3)*Z + b(4)

  dimen  <-  dim(XYZ)    
  
  if (dimen[2] !=3){
  stop('data is not 3D')
  }
  
  if (dimen[1] < 3){
  stop('too few points to fit plane')
  }
  
  # Set up constraint equations of the form  AB = 0,
  # where B is a column vector of the plane coefficients
  # in the form   b(1)*X + b(2)*Y +b(3)*Z + b(4) = 0.
  
  A  <-  cbind(XYZ, matrix(1,nrow=dimen[1] )) # Build constraint matrix
  
  ss <- svd(A)        # Singular value decomposition.
  Bs  <-  ss$v[,4]       # Solution is last column of v.
  return(Bs)             
}