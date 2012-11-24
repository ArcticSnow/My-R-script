# Simon Filhol, 28 July 2012
# Load point cloud files interactively, any 3 columns file like: .xyz or .txt file

LoadPointCloud <- function(Trans,file){
  
  ifelse(missing(file),{my.file <- file.choose()},{my.file <- file})
  # Load data into 3 column matrix
  raw.data <- read.table( my.file, quote="\"")
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
    my.info <- list(File=my.file,Translation=my.trans.xyz,Data= my.data)
  }
  else{
    my.info <- list(File=my.file,Data= my.data) 
  }
  return(my.info)
}


################################################################################################################################
################################################################################################################################
