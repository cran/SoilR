\name{getAccumulatedRelease}
\alias{getAccumulatedRelease}
\title{Calculates the accumulated carbon release from the pools as a function of time}
\description{This function computes the accumulated carbon release of the given model as funtion of time. }
\usage{getAccumulatedRelease(object)}\arguments{
  \item{object}{A Model object (e.g. of class Model or Model14)
Have a look at the methods for details.}
}
\details{This function takes a Model object, calculates the release flux as specified by \code{\link{getReleaseFlux}}, 
and integrates numerically the release flux up to each time in \code{t}.}
\value{A n x m matrix of cummulative release fluxes with m columns representing the number of pools, and n rows representing the times as specified by the argument
\code{t} in \code{\link{GeneralModel}} or other model creating functions.}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}



\seealso{See examples in \code{\link{Model}}, \code{\link{GeneralModel}}, \code{\link{GeneralModel_14}}, \code{\link{TwopParallelModel}}, 
\code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}}, etc.}

