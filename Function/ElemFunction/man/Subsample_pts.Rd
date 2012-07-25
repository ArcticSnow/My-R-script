\name{Subsample_pts}
\alias{Subsample_pts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Subsample_pts(data, dx, dy, technic, cutoff)
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
  \item{technic}{
%%     ~~Describe \code{technic} here~~
}
  \item{cutoff}{
%%     ~~Describe \code{cutoff} here~~
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
function (data, dx, dy, technic, cutoff) 
{
    Yres <- round((max(data[, 2]) - min(data[, 2]))/dy)
    Xres <- round((max(data[, 1]) - min(data[, 1]))/dx)
    a <- length(seq(from = round(min(data[, 1])), to = max(data[, 
        1]), by = dx))
    b <- length(seq(from = round(min(data[, 2])), to = max(data[, 
        2]), by = dy))
    output <- matrix(NA, a * b, 3)
    switch(technic, min = {
        k <- 1
        for (i in seq(from = round(min(data[, 1])), to = max(data[, 
            1]), by = dx)) {
            for (j in seq(from = round(min(data[, 2])), to = max(data[, 
                2]), by = dy)) {
                ind <- which((data[, 1] >= i) & (data[, 1] < 
                  (i + dx)) & (data[, 2] >= j) & (data[, 2] < 
                  (j + dy)))
                if (length(ind) < cutoff) {
                  output[k, ] <- c(NA, NA, NA)
                  rm(ind)
                } else {
                  sub.sample <- data[ind, ]
                  ind2 <- which(sub.sample[, 3] == min(sub.sample[, 
                    3]))
                  if (length(ind2 > 1)) {
                    ind2 <- sample(ind2, 1)
                  }
                  output[k, ] <- sub.sample[ind2, ]
                }
                k <- k + 1
            }
        }
        ind3 <- which(is.finite(output[, 2]))
        output <- output[ind3, ]
        return(output)
    }, max = {
        k <- 1
        for (i in seq(from = round(min(data[, 1])), to = max(data[, 
            1]), by = dx)) {
            for (j in seq(from = round(min(data[, 2])), to = max(data[, 
                2]), by = dy)) {
                ind <- which((data[, 1] >= i) & (data[, 1] < 
                  (i + dx)) & (data[, 2] >= j) & (data[, 2] < 
                  (j + dy)))
                if (length(ind) < cutoff) {
                  output[k, ] <- c(NA, NA, NA)
                  rm(ind)
                } else {
                  sub.sample <- data[ind, ]
                  ind2 <- which(sub.sample[, 3] == max(sub.sample[, 
                    3]))
                  if (length(ind2 > 1)) {
                    ind2 <- sample(ind2, 1)
                  }
                  output[k, ] <- sub.sample[ind2, ]
                }
                k <- k + 1
            }
        }
        ind3 <- which(is.finite(output[, 2]))
        output <- output[ind3, ]
        return(output)
    }, stop("This technique does not exist, only 'min' or 'max', or add new code"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
