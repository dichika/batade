\name{mkrox}
\alias{mkrox}
\title{make roxygen-style file from function code}
\usage{
  mkrox(func, pkgdir)
}
\arguments{
  \item{func}{character}

  \item{pkgdir}{nedd a full path like "paste(sep="/",
  getwd(), ''/batade/"}
}
\value{
  NULL ( a roxygen-style R source code)
}
\description{
  make roxygen-style file from function code.
}
\details{
  %% ~~ If necessary, more details than the description
  above ~~
}
\note{
  %% ~~further notes~~
}
\examples{
plot_num <- function(n){
num <- runif(n)
plot(num)
}
dir.create("sample")
dir.create("sample/R")
dir.create("sample/man")
mkrox("plot_num",pkgdir=str_c(getwd(), "/sample"))
}
\author{
  Ichikawa Daisuke
}
\references{
  %% ~put references to the literature/web site here ~
}
\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\keyword{~kwd1}
\keyword{~kwd2}

