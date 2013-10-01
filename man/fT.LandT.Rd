\name{fT.LandT}
\alias{fT.LandT}
\title{Effects of temperature on decomposition rates according to a function proposed by Lloyd and Taylor (1994)}
\description{Calculates the effects of temperature on decomposition rates according to a function proposed by Lloyd and Taylor (1994).}
\usage{fT.LandT(Temp)}
\arguments{
  \item{Temp}{A scalar or vector containing values of soil temperature for which the effects on decomposition rates are calculated}
}

\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}
\references{Lloyd, J., and J. A. Taylor (1994), On the Temperature Dependence of Soil Respiration, 
Functional Ecology, 8(3), 315-323.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}




\examples{
Temperature=0:50
plot(Temperature,fT.LandT(Temperature),type="l",
     ylab="f(T) (unitless)", 
     main="Effects of temperature on decomposition 
     rates according to the Lloyd and Taylor function")
}
