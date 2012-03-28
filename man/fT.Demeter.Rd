\name{fT.Demeter}
\alias{fT.Demeter}
\title{effects of temperature on decomposition rates according to the DEMETER model}
\description{Calculates the effects of temperature on decomposition rates according to the DEMETER model.}
\usage{fT.Demeter(Temp, Q10 = 2)}
\arguments{
  \item{Temp}{A scalar or vector containing values of temperature for which the effects on decomposition rates are calculated}
  \item{Q10}{A scalar. Temperature coefficient Q10}
}

\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}
\references{Foley, J. A. (1995), An equilibrium model of the terrestrial carbon budget, Tellus B, 47(3), 310-319.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}




\examples{
Temperature=0:50
plot(Temperature,fT.Demeter(Temperature),type="l",ylab="f(T) (unitless)", 
     main="Effects of temperature on decomposition rates according to the DEMETER model")
}
