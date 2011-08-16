\name{obj}
\alias{obj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
obj(func, pkgdir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{func}{
%%     ~~Describe \code{func} here~~
}
  \item{pkgdir}{
%%     ~~Describe \code{pkgdir} here~~
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
function(func, pkgdir){
    if(!is.character(func)){
    	stop("func must be character")
    }
    require(stringr)
    require(Rd2roxygen)
    mandir <- str_c(pkgdir, "/man/")
    codedir <- str_c(pkgdir, "/R/")
    
    #output Rd file
    Rdname <- str_c(mandir, func, ".Rd")
    obj <- eval(parse(text=func))
    prompt(obj, Rdname)

    #output roxygen code
    codename <- str_c(codedir, func, ".R")
    parse_and_save(path=Rdname, file=codename)

    #join roxygen and function code
    dump(func, file=codename, append=TRUE) 
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
