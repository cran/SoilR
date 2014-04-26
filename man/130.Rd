\name{fW.Demeter}
\alias{fW.Demeter}
\title{Effects of moisture on decomposition rates according to the DEMETER model}
\description{Calculates the effects of soil moisture on decomposition rates according to the DEMETER model.}
\usage{fW.Demeter(M, Msat = 100)}\arguments{
  \item{M}{A scalar or vector containing values of soil moisture for which the effects on decomposition rates are calculated.}
  \item{Msat}{A scalar representing saturated soil moisture.}
}

\value{A scalar or a vector containing the effects of moisture on decomposition rates (unitless).}
\references{Foley, J. A. (1995), An equilibrium model of the terrestrial carbon budget, Tellus B, 47(3), 310-319.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{
Moisture=0:100
plot(Moisture,fW.Demeter(Moisture),type="l",ylab="f(W) (unitless)", 
     main="Effects of soil moisture on decomposition rates according to the DEMETER model")
}
