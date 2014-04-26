\name{fW.Gompertz}
\alias{fW.Gompertz}
\title{Effects of moisture on decomposition rates according to the Gompertz function}
\description{Calculates the effects of water content on decomposition rates.}
\usage{fW.Gompertz(theta, a = 0.824, b = 0.308)}\arguments{
  \item{theta}{A scalar or vector containing values of volumetric soil water content.}
  \item{a}{Empirical parameter}
  \item{b}{Empirical parameter}
}


\references{I. Janssens, S. Dore, D. Epron, H. Lankreijer, N. Buchmann, B. Longdoz, J. Brossaud, L. Montagnani. 2003.
Climatic Influences on Seasonal and Spatial Differences in Soil CO2 Efflux. In Valentini, R. (Ed.)
Fluxes of Carbon, Water and Energy of European Forests. pp 235-253. Springer. }
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{
th=seq(0,1,0.01)
xi=fW.Gompertz(theta=th)
plot(th,xi,type="l",main="Effects of soil water content on decomposition rates",
     xlab="Volumetric soil water content (cm3 cm-3)",ylab=expression(xi))
}
