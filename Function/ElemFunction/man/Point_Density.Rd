\name{Point_Density}
\alias{Point_Density}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Point_Density(data, dx, dy, Xlim, Ylim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{dx}{
%%     ~~Describe \code{dx} here~~
}
  \item{dy}{
%%     ~~Describe \code{dy} here~~
}
  \item{Xlim}{
%%     ~~Describe \code{Xlim} here~~
}
  \item{Ylim}{
%%     ~~Describe \code{Ylim} here~~
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
function (data, dx, dy, Xlim, Ylim) 
{
    library(MASS)
    x.res <- seq(from = Xlim[1], to = Xlim[2] - dx, by = dx)
    y.res <- seq(from = Ylim[1], to = Ylim[2] - dy, by = dy)
    z.density <- matrix(NA, nrow = length(x.res), ncol = length(y.res))
    k <- 1
    for (i in x.res) {
        m <- 1
        for (j in y.res) {
            ind <- which((data[, 1] >= i) & (data[, 1] < (i + 
                dx)) & (data[, 2] >= j) & (data[, 2] < (j + dy)))
            z.density[k, m] <- length(ind)/(dx * dy * 10 * 10)
            m <- m + 1
        }
        k <- k + 1
    }
    density <- list(x = x.res + dx/2, y = y.res + dy/2, z = z.density)
    print(cat("min and max density =", c(min(density$z), max(density$z))))
    layout(t(c(1, 1, 1, 1, 1, 2, 2)))
    image(density$x, density$y, log(density$z), xlab = "x (m)", 
        ylab = "y (m)", main = "Point density of raw data in pts/dm2")
    contour(density, add = TRUE, labcex = 1.2)
    hist(log(density$z), main = "Histogram of point density", 
        xlab = "Point density")
    return(density)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
