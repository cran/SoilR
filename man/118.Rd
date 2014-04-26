\name{fT.Daycent2}
\alias{fT.Daycent2}
\title{Effects of temperature on decomposition rates according to the DAYCENT model}
\description{Calculates the effects of temperature on decomposition rates according to the Daycent/Century models.}
\usage{fT.Daycent2(Temp)}\arguments{
  \item{Temp}{A scalar or vector containing values of soil temperature for which the effects on decomposition rates are calculated.}
}

\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}
\references{Del Grosso, S. J., W. J. Parton, A. R. Mosier, E. A. Holland, E. Pendall, D. S. Schimel, and D. S. Ojima (2005), 
Modeling soil CO2 emissions from ecosystems, Biogeochemistry, 73(1), 71-91.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{
Temperature=0:50
plot(Temperature,fT.Daycent2(Temperature),type="l",
     ylab="f(T) (unitless)",
     main="Effects of temperature on decomposition rates according to the DAYCENT model")
}
