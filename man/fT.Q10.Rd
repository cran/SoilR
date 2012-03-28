\name{fT.Q10}
\alias{fT.Q10}
\title{Effects of temperature on decomposition rates according to a Q10 function}
\description{Calculates the effects of temperature on decomposition rates according to a Q10 function.}
\usage{fT.Q10(Temp, a = 1, Q10 = 2)}
\arguments{
  \item{Temp}{A scalar or vector containing values of temperature for which the effects on decomposition rates are calculated.}
  \item{a}{A scalar.}
  \item{Q10}{A scalar. Temperature coefficient Q10.}
}

\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}




\examples{
Temperature=0:50
plot(Temperature,fT.Q10(Temperature),type="l",ylab="f(T) (unitless)", 
     main="Effects of temperature on decomposition rates according to a Q10 function")
lines(Temperature, fT.Q10(Temperature,Q10=2.2),col=2)
lines(Temperature, fT.Q10(Temperature,Q10=1.4),col=4)
legend("topleft",c("Q10 = 2", "Q10 = 2.2", "Q10 = 1.4"),lty=c(1,1,1),col=c(1,2,4),bty="n")
}
