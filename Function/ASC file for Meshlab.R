# by Simon Filhol
# 
# version 0.1 19July2012
# 
# Function converting XYZ data into ASCII file format compatible with Meshlab.

# data = 3-column or more matrix XYZ or XYZRGB
# filename = character for the filename with .asc at the end. example: "filename.asc"


ASCtoMeslab <- function(data,filename){
  direct <- getwd()
  print(direct)
  
  library("tcltk")
  my.dir.orig <- tk_choose.dir(default = direct, caption = "Select the destination file directory")
  setwd(my.dir.orig)
  
  write.table(data,file=filename,sep=",",col.names=FALSE,row.names=FALSE)
  
}
