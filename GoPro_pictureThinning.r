# By Simon Filhol, 19th June 2012.
# Script to thin picture files for GoPro time-lapse

# my.new.folder = name for a new folder where picture files will be copied

GoPro_ThinPictureFiles <- function(my.new.folder){
  
  # Select interactively the folder where the picutres are located
  library("tcltk")
  my.dir.orig <- tk_choose.dir(default = "/Users/simonfilhol", caption = "Select the pictures directory")
  my.file.name <- dir(my.dir.orig)
  setwd(my.dir.orig)
  #my.mtime <- matrix(NA,nrow=(length(my.file.name)))  
  
  # Read file information and retain time of creation
  my.file.info <- file.info(c(paste(my.dir.orig,my.file.name,sep="/")))
  my.mtime <- my.file.info$mtime
  
  my.mtime.line <- my.mtime-my.mtime[1]
  my.mtime.diff <- diff(my.mtime.line)
  
  # Resume few statistics of the file's timeshot
  print(paste("Number of Pictures =",length(my.mtime)))
  print("Summary of time steps (in second) =")
  print(summary(as.numeric(my.mtime.diff)))
  print(paste("Total time of GoPro run =",max(as.numeric(my.mtime.line))))
  
  # # ask to plot histogram of time gap between shots
  # graph <- readline("Do you want to plot histogram (0/1)?")
  # if(graph==1){
  # #hist(as.numeric(my.mtime.diff))
  # plot(my.mtime.line,rep(1,times=length(my.mtime.line)),pch=124,ylab="",xlab="Time in s")
  # }
  # rm(graph)
  
  # Find big time step gap in according to outlier choice.
  my.outlier <- as.numeric(readline("Treshold of time step considered as outlier? (in second) "))
  
  
  # ask for thinning until 1/my.thin is integer. This will force th program to only copy file a regular time step
  repeat{
    my.thin <- as.numeric(readline("What file thinning do you want (i.e. 1/2,1/4, 1/10... in numeric form)? "))
    print(1/my.thin)
    if(is.wholenumber(1/my.thin)){
      break
      }
    else{
      print(" 1 dvided by thin parameter must be an integer")
    }
  }
  my.ind.vec <- seq(from=1, to=length(my.mtime.line),by=1/my.thin)
  print(paste("New time step is: ",median(as.numeric(my.mtime.diff))/my.thin," second"))
  print(paste("Number of file to copy: ",length(my.ind.vec)))
  
  #Create new folder and copy files into folder
  my.new.dir <- tk_choose.dir(default = my.dir.orig, caption = "Select the destination directory")
  
  my.dir.final <- paste(my.new.dir,my.new.folder,sep="/")
  dir.create(paste(my.dir.final),showWarnings=TRUE)
  file.copy(from=paste(my.dir.orig,my.file.name[my.ind.vec],sep="/"), to=my.dir.final, overwrite = FALSE, recursive = FALSE,copy.mode = TRUE)
  setwd(my.dir.final)
  
  # Rename files in consecutive order, ready for Photoshop
  start <- as.numeric(readline("Give a new file number to the first copied file: "))
  new.file.name=paste("TM",c(start:(start+length(my.ind.vec)-1)),".jpg",sep="")
  file.rename(from=my.file.name[my.ind.vec],to=new.file.name)
  
  # Write text file with information about the files
  setwd(my.new.dir)
  ind <- which(my.mtime.diff>my.outlier)
  if(length(ind)>0){
    outlier.name <- paste(my.new.folder,"_outlier.txt")
    write(c("1st file name","2nd file name","Timestep"),file=oultier.name,ncolumns=3,sep="\t")
    write(rbind(my.file.name[ind],my.file.name[ind+1],my.mtime.diff[ind]),file=oultier.name,ncolumns=3,sep="\t",append=TRUE)
  }
  
  info.name <- paste(my.new.folder,"_info.txt")
  write(c("TM_file.name","GoPro_file.name","Time_shot","Diff_with.next"),file=info.name,ncolumns=4,sep="\t")
  
  time.diff <- (diff(my.mtime.line[my.ind.vec]))
  time.diff <- c(as.numeric(time.diff),0)
  
  write(rbind(new.file.name,my.file.name[my.ind.vec],as.character(my.file.info[my.ind.vec,4]),time.diff),file=info.name,ncolumns=4,sep="\t",append=TRUE)
  
  my.text <- paste(length(my.ind.vec)," pictures have been copied\n  from= \n",my.dir.orig, "\n  to= \n", my.dir.final)
  writeLines(my.text)
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){ abs(x - round(x)) < tol}



