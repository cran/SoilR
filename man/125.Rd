\name{fT.linear}
\alias{fT.linear}
\title{Effects of temperature on decomposition rates according to a linear model}
\description{Calculates the effects of temperature on decomposition rates according to a linear model.}
\usage{fT.linear(Temp, a = 0.198306, b = 0.036337)}\arguments{
  \item{Temp}{A scalar or vector containing values of temperature for which the effects on decomposition rates are calculated.}
  \item{a}{A scalar defining the intercept of the linear function.}
  \item{b}{A scalar defining the slope of the linear function.}
}

\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}
\references{Adair, E. C., W. J. Parton, S. J. D. Grosso, W. L. Silver, M. E. Harmon, S. A. Hall, I. C. Burke, and S. C. Hart. 2008. 
Simple three-pool model accurately describes patterns of long-term litter decomposition in diverse climates. Global Change Biology 14:2636-2660.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{
Temperature=0:50
plot(Temperature,fT.linear(Temperature),type="l",ylab="f(T) (unitless)", 
     main="Effects of temperature on decomposition rates according to a linear function")
}
