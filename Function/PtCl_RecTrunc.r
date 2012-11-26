# Simon Filhol, 25th November 2012

# function for the script Sprocess_liDAR_subselection2_winter1112.r
# Can be generalized in further version by adding ... instead of A,B,C,D,FF,G,H and combining the .. into a list


PtCl_RecTrunc <- function (A,B,C,D,E,FF,G,H,xlim,ylim) {
  
  win.xlim <- xlim
  win.ylim <- ylim
  win.xyz.A <- A$xyz[A$xyz[,1]>win.xlim[1] & A$xyz[,1]<win.xlim[2],] 
  win.xyz.A <- win.xyz.A[win.xyz.A[,2]>win.ylim[1] & win.xyz.A[,2]<win.ylim[2],]
  
  win.xyz.B <- B$xyz[B$xyz[,1]>win.xlim[1] & B$xyz[,1]<win.xlim[2],] 
  win.xyz.B <- win.xyz.B[win.xyz.B[,2]>win.ylim[1] & win.xyz.B[,2]<win.ylim[2],]
  
  win.xyz.C <- C$xyz[C$xyz[,1]>win.xlim[1] & C$xyz[,1]<win.xlim[2],] 
  win.xyz.C <- win.xyz.C[win.xyz.C[,2]>win.ylim[1] & win.xyz.C[,2]<win.ylim[2],]
  
  win.xyz.D <- D$xyz[D$xyz[,1]>win.xlim[1] & D$xyz[,1]<win.xlim[2],] 
  win.xyz.D <- win.xyz.D[win.xyz.D[,2]>win.ylim[1] & win.xyz.D[,2]<win.ylim[2],]
  
  win.xyz.E <- E$xyz[E$xyz[,1]>win.xlim[1] & E$xyz[,1]<win.xlim[2],] 
  win.xyz.E <- win.xyz.E[win.xyz.E[,2]>win.ylim[1] & win.xyz.E[,2]<win.ylim[2],]
  
  win.xyz.F <- FF$xyz[FF$xyz[,1]>win.xlim[1] & FF$xyz[,1]<win.xlim[2],] 
  win.xyz.F <- win.xyz.F[win.xyz.F[,2]>win.ylim[1] & win.xyz.F[,2]<win.ylim[2],]
  
  win.xyz.G <- G$xyz[G$xyz[,1]>win.xlim[1] & G$xyz[,1]<win.xlim[2],] 
  win.xyz.G <- win.xyz.G[win.xyz.G[,2]>win.ylim[1] & win.xyz.G[,2]<win.ylim[2],]
  
  win.xyz.H <- H$xyz[H$xyz[,1]>win.xlim[1] & H$xyz[,1]<win.xlim[2],] 
  win.xyz.H <-  win.xyz.H[win.xyz.H[,2]>win.ylim[1] & win.xyz.H[,2]<win.ylim[2],]
  
  win <- data.frame(
    Date=factor(c(rep("23 Sept",times=dim(win.xyz.A)[1]),
                  rep("23 Oct",times=dim(win.xyz.B)[1]),
                  rep("11 Nov",times=dim(win.xyz.C)[1]),
                  rep("20 Dec",times=dim(win.xyz.D)[1]),
                  rep("11 Jan",times=dim(win.xyz.E)[1]),
                  rep("15 Fev",times=dim(win.xyz.F)[1]),
                  rep("8 Mars",times=dim(win.xyz.G)[1]),
                  rep("26 Mars",times=dim(win.xyz.H)[1])         
    ), levels=c('23 Sept', '23 Oct', '11 Nov','20 Dec','11 Jan','15 Fev','8 Mars','26 Mars')),
    x=c(win.xyz.A[,1],win.xyz.B[,1],win.xyz.C[,1],win.xyz.D[,1],
        win.xyz.E[,1],win.xyz.F[,1],win.xyz.G[,1],win.xyz.H[,1]),
    y=c(win.xyz.A[,2],win.xyz.B[,2],win.xyz.C[,2],win.xyz.D[,2],
        win.xyz.E[,2],win.xyz.F[,2],win.xyz.G[,2],win.xyz.H[,2]),
    z=c(win.xyz.A[,3],win.xyz.B[,3],win.xyz.C[,3],win.xyz.D[,3],
        win.xyz.E[,3],win.xyz.F[,3],win.xyz.G[,3],win.xyz.H[,3]))
return(win)
}

plot.slice <- function(win,xlim,ylim){
  require(ggplot2)
  win1 <- win
  win1.xlim <- xlim
  win1.ylim <- ylim
h <- qplot(win1$x,win1$z,colour=win1$Date,alpha=I(1/2),size = I(3),geom="point",
      asp=(diff(win1.ylim)+.2)/(diff(win1.xlim)+.2),xlim=c(win1.xlim[1]-.1,win1.xlim[2]+.1),ylim=c(-0.25,1.25),
      xlab="Distance (m)",ylab="Elevation (m)",
      main="Slice 10cm Thick From Original Point Cloud")+scale_colour_discrete(name = "Scanning Date")+ 
  scale_x_continuous(breaks = round(seq(min(win1$x), max(win1$x), by = 0.2),1)) +
  scale_y_continuous(breaks = round(seq(min(win1$z), max(win1$z), by = 0.2),1))
return(h)
}