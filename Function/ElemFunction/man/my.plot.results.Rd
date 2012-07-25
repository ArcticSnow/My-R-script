\name{my.plot.results}
\alias{my.plot.results}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
my.plot.results(my.lonlats, my.values, my.dxdy, str, Xlim, Ylim, leg.titl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my.lonlats}{
%%     ~~Describe \code{my.lonlats} here~~
}
  \item{my.values}{
%%     ~~Describe \code{my.values} here~~
}
  \item{my.dxdy}{
%%     ~~Describe \code{my.dxdy} here~~
}
  \item{str}{
%%     ~~Describe \code{str} here~~
}
  \item{Xlim}{
%%     ~~Describe \code{Xlim} here~~
}
  \item{Ylim}{
%%     ~~Describe \code{Ylim} here~~
}
  \item{leg.titl}{
%%     ~~Describe \code{leg.titl} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (my.lonlats, my.values, my.dxdy, str, Xlim, Ylim, leg.titl) 
{
    if (length(my.values == 2)) {
        layout(1)
        Xmin = Xlim[1]
        Xmax = Xlim[2]
        Ymin = Ylim[1]
        Ymax = Ylim[2]
        my.lons <- seq(Xmin, Xmax, my.dxdy)
        my.lats <- seq(Ymin, Ymax, my.dxdy)
        my.grays <- gray(5:59/63)
        my.grays[length(my.grays)] <- "white"
        borders <- cbind(c(Xmin, Xmax, Xmax, Xmin), c(Ymin, Ymin, 
            Ymax, Ymax))
        in.border <- point.in.polygon(my.lonlats[, 1], my.lonlats[, 
            2], borders[, 1], borders[, 2])
        my.results <- matrix(NA, nrow = length(my.lons), ncol = length(my.lats))
        my.results[in.border != 0] <- my.values
        par(mar = c(4, 4, 4, 4))
        image(my.lons, my.lats, my.results, col = my.grays, main = str, 
            cex.main = 1.5, xlab = "x (m)", ylab = "Y (m)")
        contour(my.lons, my.lats, my.results, add = T, labcex = 1.2)
        legend.col(col = gray(5:59/63), lev = my.results, title = leg.titl)
        return(my.results)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
