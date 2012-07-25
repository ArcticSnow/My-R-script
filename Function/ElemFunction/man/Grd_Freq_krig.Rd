\name{Grd_Freq_krig}
\alias{Grd_Freq_krig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Grd_Freq_krig(data.entry, Xlim, Ylim, dx, dy, my.krig.dxdy)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.entry}{
%%     ~~Describe \code{data.entry} here~~
}
  \item{Xlim}{
%%     ~~Describe \code{Xlim} here~~
}
  \item{Ylim}{
%%     ~~Describe \code{Ylim} here~~
}
  \item{dx}{
%%     ~~Describe \code{dx} here~~
}
  \item{dy}{
%%     ~~Describe \code{dy} here~~
}
  \item{my.krig.dxdy}{
%%     ~~Describe \code{my.krig.dxdy} here~~
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
function (data.entry, Xlim, Ylim, dx, dy, my.krig.dxdy) 
{
    data <- matrix(c(data.entry[, 1], data.entry[, 2], data.entry[, 
        3]), ncol = 3)
    data <- Truncate_XY(data, Xlim, Ylim)
    res = (Xlim[2] - Xlim[1])/dx
    my.pt.density <- Point_Density(data, res, Xlim, Ylim)
    smin <- Subsample_pts(data, dx, dy, "min")
    plot(smin[, 1], smin[, 2])
    library(akima)
    my.xo <- seq(from = Xlim[1], to = Xlim[2], by = my.krig.dxdy)
    my.yo <- seq(from = Ylim[1], to = Ylim[2], by = my.krig.dxdy)
    my.interp <- interp(smin[, 1], smin[, 2], smin[, 3], my.xo, 
        my.yo, linear = TRUE)
    my.grays <- gray(5:59/64)
    layout(1)
    image(my.interp, col = my.grays)
    contour(my.interp, add = TRUE, labcex = 1.5)
    library(geoR)
    my.geodata <- as.geodata(matrix(c(smin[, 1], smin[, 2], smin[, 
        3]), ncol = 3))
    my.robust.vario.directional <- variog4(my.geodata, trend = "2nd", 
        estimator.type = "modulus", direction = c(0, pi/4, pi/2, 
            3 * pi/4), tolerance = pi/8, max.dist = 1.5)
    plot(my.robust.vario.directional)
    my.robust.vario <- variog(my.geodata, trend = "2nd", estimator.type = "modulus", 
        max.dist = 1.5)
    plot(my.robust.vario)
    print("What semivariogram to use?")
    switch(menu(c("Exponential", "Spherical", "Gaussian")), c = {
        c <- "exponential"
    }, c = {
        c <- "spherical"
    }, c = {
        c <- "gaussian"
    })
    my.var.fit <- variofit(my.robust.vario, ini.cov.pars = c(0.005, 
        0.025), cov.model = c, fix.nugget = FALSE, nugget = 0.005, 
        weights = "equal", max.dist = 1.5)
    lines(my.var.fit)
    my.tausq <- my.var.fit$nugget
    my.sigsq <- my.var.fit$cov.pars[1]
    my.phi <- my.var.fit$cov.pars[2]
    my.grid <- pred_grid(Xlim, Ylim, by = my.krig.dxdy)
    my.kr.obj <- krige.control(type.krige = "OK", cov.model = c, 
        cov.pars = c(my.sigsq, my.phi), nugget = my.tausq)
    my.kr.results <- krige.conv(my.geodata, locations = my.grid, 
        krige = my.kr.obj)
    my.kr.sd <- sqrt(my.kr.results$krige.var)
    my.kr.pred <- my.kr.results$predict
    my.results <- my.plot.results(my.grid, my.kr.pred, my.krig.dxdy, 
        "Universal Kriging", Xlim = Xlim, Ylim = Ylim)
    All.resutls <- list(my.pt.density)
    return(All.results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
