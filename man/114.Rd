\name{fT.Arrhenius}
\alias{fT.Arrhenius}
\title{Effects of temperature on decomposition rates according the Arrhenius equation}
\description{Calculates the effects of temperature on decomposition rates according to the Arrhenius equation.}
\usage{fT.Arrhenius(Temp, A = 1000, Ea = 75000, Re = 8.3144621)}\arguments{
  \item{Temp}{A scalar or vector containing values of temperature (in degrees Kelvin) for which the effects on decomposition rates are calculated.}
  \item{A}{A scalar defining the pre-exponential factor.}
  \item{Ea}{A scalar defining the activation energy in units of J mol^-1.}
  \item{Re}{A scalar defining the universal gas contant in units of J K^-1 mol^-1.}
}

\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{
Temperature=273:300
plot(Temperature,fT.Arrhenius(Temperature),type="l",ylab="f(T) (unitless)", xlab="Temperature (K)",
     main="Effects of temperature on decomposition rates according to the Arrhenius equation")
}
