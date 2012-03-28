\name{HarvardForest14CO2}
\alias{HarvardForest14CO2}
\docType{data}
\title{
 Soil 14CO2 efflux from Harvard Forest
}
\description{
    Radiocarbon measured between 1996 and 2010 in soil respired CO2 for well-drained soils in the footprint of the Harvard forest tower.
}
\usage{data(HarvardForest14CO2)}
\format{
  A data frame with the following 3 variables.
  \describe{
    \item{\code{Year}}{A numeric vector with the date of measurement in years}
    \item{\code{D14C}}{A numeric vector with the value of the Delta 14C value measured in CO2 efflux in per mil}
    \item{\code{Site}}{A factor indicating the site where measurements were made. NWN: Northwest Near, Drydown: Rainfall exclusion experiment.}
  }
}
\references{Sierra, C. A., Trumbore, S. E., Davidson, E. A., Frey, S. D., Savage, K. E., and Hopkins, F. M. 2012. Predicting decadal trends and transient responses of radiocarbon storage and fluxes in a temperate forest soil, Biogeosciences Discuss., 9, 2197-2232, doi:10.5194/bgd-9-2197-2012}
\examples{
data(HarvardForest14CO2)
plot(HarvardForest14CO2[,1],HarvardForest14CO2[,2])
}
\keyword{datasets}
