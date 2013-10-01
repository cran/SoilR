\name{C14Atm}
\alias{C14Atm}
\docType{data}
\title{
$^{14}C$  fraction in per mille 
}
\description{
    The usually used data set for the atmospheric value of the $^{14}C$  fraction in per mille
}
\usage{data(C14Atm)}
\format{
  A data frame with 108 observations on the following 2 variables.
  \describe{
    \item{\code{V1}}{a numeric vector}
    \item{\code{V2}}{a numeric vector}
  }
}
\examples{
#Notice that C14Atm is a shorter version of C14Atm_NH
plot(C14Atm_NH,type="l")
lines(C14Atm,col=2)
}
\keyword{datasets}
