% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hua2021.R
\docType{data}
\name{Hua2021}
\alias{Hua2021}
\title{Atmospheric radiocarbon for the period 1950-2019 from Hua et al. (2021)}
\format{
A \link{list} containing 5 data frames, each representing an atmospheric zone. The zones are: 
  NHZone1: northern hemisphere zone 1, NHZone2: northern hemisphere zone 2,
  NHZone3: northern hemisphere zone 3, SHZone1-2: southern hemisphere zones 1 and 2, 
  SHZone3: southern hemisphere zone 3. Each data frame contains a variable number of observations 
  on the following 5 variables.
  \describe{
    \item{\code{Year}}{Year AD}
    \item{\code{mean.Delta14C}}{mean value of atmospheric radiocarbon reported as Delta14C}
    \item{\code{sd.Delta14C}}{standard deviation of atmospheric radiocarbon reported as Delta14C}
    \item{\code{mean.F14C}}{mean value of atmospheric radiocarbon reported as fraction modern F14C}
    \item{\code{sd.F14C}}{standard deviation of atmospheric radiocarbon reported as fraction modern F14C}
  }
}
\source{
\doi{10.1017/RDC.2021.95}
}
\usage{
data(Hua2013)
}
\description{
Atmospheric radiocarbon for the period 1950-2019 reported by Hua et al. (2020) for 5 atmospheric zones.
}
\details{
This dataset corresponds to Supplementary Material 2 from Hua et al. (2021). For additional details see the original publication.
}
\examples{
plot(Hua2021$NHZone1[,1:2],type="l")
lines(Hua2021$NHZone2[,1:2],col=2)
lines(Hua2021$NHZone3[,1:2],col=3)
lines(Hua2021$`SHZone1-2`[,1:2],col=4)
lines(Hua2021$SHZone3[,1:2],col=5)
legend("topright",names(Hua2021), col=1:5,lty=1,bty="n")

}
\references{
Hua, Q., Turnbull, J., Santos, G., Rakowski, A., Ancapichun, S., De Pol-Holz, R., . . . Turney, C. (2021). ATMOSPHERIC RADIOCARBON FOR THE PERIOD 1950–2019. Radiocarbon, 1-23. doi:10.1017/RDC.2021.95
}
\author{
Carlos A. Sierra \email{csierra@bgc-jena.mpg.de}
}
\keyword{datasets}
