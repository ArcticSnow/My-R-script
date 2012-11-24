# Simon Filhol, 28 July 2012
# Load point cloud files interactively, any 3 columns file like: .xyz or .txt file

LoadPointCloud <- function(Trans){
  
  library("tcltk")
  
  # Test the type of OS, windows or mac
  if(Sys.info()['sysname']=="Windows"){
    my.dir.orig <- tk_choose.dir(default = "C:/Documents and Settings", caption = "Select the file directory")
  }
  if(Sys.info()['sysname']=="Darwin"){
    my.dir.orig <- tk_choose.dir(default = "/Users", caption = "Select the file directory")
  }
  if(is.na(my.dir.orig)){break}
  my.file.name <- dir(my.dir.orig)
  
  
  repeat {
    print("Which file to use? ")
    print(my.file.name)
    ind <- as.numeric(readline("Give row number for file to open: "))
    my.file <- my.file.name[ind]
    print(my.file)
    OK <- as.numeric(readline("Is it the right file (0/1)? "))
    if(OK>0){break}
  }
  
  
  # Load data into 3 column matrix
  raw.data <- read.table( paste(my.dir.orig,"/",my.file,sep=""), quote="\"")
  my.data <- matrix(c(raw.data[,1],raw.data[,2],raw.data[,3]),ncol=3)
  rm(raw.data)
  my.data <- round(my.data,3)
  
  # Translate data to reduce memory use
  
  if(my.data[1,1]>500){
    if(hasArg(Trans)){
      my.trans.xyz <- Trans
    }
    else{
      my.trans.xyz <- my.data[1,]
    }
    print(paste("Translation apply =",my.trans.xyz))
    my.data <- cbind(my.data[,1]-my.trans.xyz[1], my.data[,2]-my.trans.xyz[2],my.data[,3]-my.trans.xyz[3])
    my.info <- list(Directory=my.dir.orig,Filelist=my.file.name,Translation=my.trans.xyz,Data= my.data)
  }
  else{
    my.info <- list(Directory=my.dir.orig,Filelist=my.file.name,Data= my.data) 
  }
  return(my.info)
}


################################################################################################################################
################################################################################################################################
