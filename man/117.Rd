\name{fT.Daycent1}
\alias{fT.Daycent1}
\title{Effects of temperature on decomposition rates according to the DAYCENT model}
\description{Calculates the effects of temperature on decomposition rates according to the DAYCENT model.}
\usage{fT.Daycent1(Temp)}\arguments{
  \item{Temp}{A scalar or vector containing values of soil temperature for which the effects on decomposition rates are calculated}
}

\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}
\references{Kelly, R. H., W. J. Parton, M. D. Hartman, L. K. Stretch, D. S. Ojima, and D. S. Schimel (2000), 
Intra-annual and interannual variability of ecosystem processes in shortgrass steppe, J. Geophys. Res., 105.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{
Temperature=0:50
plot(Temperature,fT.Daycent1(Temperature),type="l",ylab="f(T) (unitless)", 
     main="Effects of temperature on decomposition rates according to the DAYCENT model")
}
