\name{LoadPointCloud}
\alias{LoadPointCloud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LoadPointCloud()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    library("tcltk")
    if (Sys.info()["sysname"] == "Windows") {
        my.dir.orig <- tk_choose.dir(default = "C:/Documents and Settings", 
            caption = "Select the file directory")
    }
    if (Sys.info()["sysname"] == "Darwin") {
        my.dir.orig <- tk_choose.dir(default = "/Users", caption = "Select the file directory")
    }
    if (is.na(my.dir.orig)) {
        break
    }
    my.file.name <- dir(my.dir.orig)
    repeat {
        print("Which file to use? ")
        print(my.file.name)
        ind <- as.numeric(readline("Give row number for file to open: "))
        my.file <- my.file.name[ind]
        print(my.file)
        OK <- as.numeric(readline("Is it the right file (0/1)? "))
        if (OK > 0) {
            break
        }
    }
    raw.data <- read.table(paste(my.dir.orig, "/", my.file, sep = ""), 
        quote = "\"")
    my.data <- matrix(c(raw.data[, 1], raw.data[, 2], raw.data[, 
        3]), ncol = 3)
    rm(raw.data)
    my.data <- round(my.data, 3)
    if (my.data[1, 1] > 500) {
        my.trans.xyz <- my.data[1, ]
        print(paste("Translation apply =", my.trans.xyz))
        my.data <- cbind(my.data[, 1] - my.trans.xyz[1], my.data[, 
            2] - my.trans.xyz[2], my.data[, 3] - my.trans.xyz[3])
        my.info <- list(Directory = my.dir.orig, Filelist = my.file.name, 
            Translation = my.trans.xyz, Data = my.data)
    }
    else {
        my.info <- list(Directory = my.dir.orig, Filelist = my.file.name, 
            Data = my.data)
    }
    return(my.info)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
