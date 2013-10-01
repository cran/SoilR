\name{eCO2}
\alias{eCO2}
\docType{data}
\title{
 Soil CO2 efflux from incubation experiments
}
\description{
Two laboratory incubation experiments where performed in April-May 2013 for a period of 42 days. Soil CO2 measurements were taken under controlled laboratory conditions. One soil was sampled at a boreal forest site (Caribou Poker Research Watershed) and the other from a temperate forest in Germany (Hainich National Park). For each soil 5 replicates were incubated and sampled. This dataset presents the mean and standard deviation of the measurements.
}
\usage{data(eCO2)}
\format{
  A data frame with the following 4 variables.
  \describe{
    \item{\code{Sample}}{Sample code: AK_T25 is a soil from a boreal forest in Alaska incubated at 25 degrees C. HN_T35 is a soil from a temperate German forest (Hainich National Park) incubated at 35 degrees C.}
    \item{\code{Days}}{A numeric vector with the day of measurement.}
    \item{\code{eCO2mean}}{A numeric vector with the accumulated release of CO2. Units in mg C g-1 soil C.}
    \item{\code{eCO2sd}}{A numeric vector with the standard deviation of the accumulated release of CO2. Units in mg C g-1 soil C.}
  }
}
\examples{
head(eCO2)

points(eCO2mean~Days,data=eCO2,subset=Sample=="AK_T25",col=4)
legend("topleft",
       c("HN_T35: temperate forest soil","AK_T25: boreal forest soil"),
       pch=1,col=c(2,4),bty="n")
}
\keyword{datasets}
