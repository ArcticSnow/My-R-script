# Simon Filhol, 28 July 2012
# Load point cloud files interactively, any 3 columns file like: .xyz or .txt file

LoadPointCloud <- function(){

library("tcltk")
my.dir.orig <- tk_choose.dir(default = "/Users/simonfilhol", caption = "Select the pictures directory")
my.file.name <- dir(my.dir.orig)

OK <- 0
while (OK==0){
  print("Which file to use? ")
print(my.file.name)
ind <- as.numeric(readline("Give row number for file to open: "))
my.file <- my.file.name[ind]
print(my.file)
OK <- as.numeric(readline("Is it the right file (0/1)? "))
}


# Load data into 3 column matrix
raw.data <- read.table( paste(my.dir.orig,"/",my.file,sep=""), quote="\"")
my.data <- matrix(c(raw.data[,1],raw.data[,2],raw.data[,3]),ncol=3)
rm(raw.data)

# Translate data to reduce memory use
if(my.data[1,1]>500){
my.trans.xyz <- my.data[1,]
print(my.trans.xyz)
my.data <- cbind(my.data[,1]-my.trans.xyz[1], my.data[,2]-my.trans.xyz[2],my.data[,3]-my.trans.xyz[3])
my.info <- list(Translation=my.trans.xyz,Data= my.data)
}
else{
  my.info <- list(Data= my.data) 
}
return(my.info)
}

################################################################################################################################
################################################################################################################################
