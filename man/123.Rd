\name{fT.RothC}
\alias{fT.RothC}
\title{Effects of temperature on decomposition rates according to the functions included in the RothC model}
\description{Calculates the effects of temperature on decomposition rates according to the functions included in the RothC model.}
\usage{fT.RothC(Temp)}\arguments{
  \item{Temp}{A scalar or vector containing values of temperature for which the effects on decomposition rates are calculated.}
}

\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}
\references{Jenkinson, D. S., S. P. S. Andrew, J. M. Lynch, M. J. Goss, and P. B. Tinker (1990), 
The Turnover of Organic Carbon and Nitrogen in Soil, Philosophical Transactions: Biological Sciences, 329(1255), 361-368.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}
\note{This function returns NA for Temp <= -18.3}



\examples{
Temperature=0:50
plot(Temperature,fT.RothC(Temperature),type="l",ylab="f(T) (unitless)", 
     main="Effects of temperature on decomposition rates according to the RothC model")
}
