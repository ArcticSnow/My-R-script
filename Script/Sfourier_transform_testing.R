
x <- 0:1:100
y <- 0:1:100
Z1 <- matrix(1,ncol=length(x),nrow=length(y))
Z2 <- matrix(1,ncol=length(x),nrow=length(y))
Z3 <- matrix(1,ncol=3,nrow=length(x)*length(y))
k <- 0
for (i in 1:length(x)){
  for (j in 1:length(y)){
    k <- k+1
    Z1[j,i] <- cos(x[i]+2)+2*cos(20*y[j])
    Z2[j,i] <- sin(x[i])+sin(y[j])
    Z3[k,] <- cbind(x[i],y[j],  cos(x[i]+2)+2*cos(20*y[j])+2*x[i])
  }
}
rm(k)
image(Z1)
image(Z2)

image(log10(Fourier.trans.1),col=gray(0:50/50))
image(Re(mvfft(Z1)))


Temp=seq(from=0,to=100,by=0.1)
Time=Temp*




