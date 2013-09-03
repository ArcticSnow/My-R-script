# Base code by Margaret Short, transformed into function by Filhol S. on 3rd MAy 2012
# Do we need a spatial model?
# 
# Idea:
#   
#   1) Reorder the data many times, calculate and plot empirical 
# semivariograms.
# 
# 2) Overlay our empirical semivariogram. Compare.
# 


Test.sp.model <- function(my.vario,my.geodata,trend.tec,titl){
  library(geoR)
plot(my.vario, main = "", pts.range.cex = c(1,3), type='b',cex.lab=1.5, cex.axis=1.5)
  title(main=titl)

my.longs <- my.geodata$coords[,1]
my.lats <- my.geodata$coords[,2]

my.new.geo <- my.geodata$data
#my.log.my.geodata <- my.geodata$data # remember, logged my.geodata earlier

n <- length( my.geodata$coords )

for( i in 1:100 ) {
  my.new.order <- sample( 1:n, size=n, repl=FALSE )
  my.reordered <- my.new.geo[my.new.order]
  my.reordered.geo <- as.geodata(cbind(my.longs,my.lats,my.reordered),coords.col = 1:2, data.col = 3)
  my.v.new <- variog(my.reordered.geo,trend=trend.tec,estimator.type="modulus")
  lines(my.v.new, col="gray")
}
lines(my.vario,lwd=3) # overlay our semi-v'gram in thick, black

}

# Example:
#   Test.sp.model(my.robust.vario,my.geodata,"2nd","b")











