\name{getReleaseFlux}
\alias{getReleaseFlux}
\title{Calculates the release of C from each pool}
\description{This function computes carbon release from each pool of the given model as funtion of time 
Have a look at the methods for details.}
\usage{getReleaseFlux(object)}\arguments{
  \item{object}{An model object (the actual class depends on the method e.g. Model or  Model14}
}
\details{This function takes a Model object, which represents a system of ODEs 
solves the system for \eqn{\mathbf{C}(t)}{C(t)}, calculates a diagonal matrix of release coefficients \eqn{\mathbf{R}(t)}{R(t)}, 
and computes the release flux as \eqn{\mathbf{R}(t) \mathbf{C}(t)}{R(t) C(t)}.
The numerical solver used can be specified in the model creating functions like e.g. \code{\link{Model}}.}
\value{A matrix. Every column represents a pool and every row a point in time}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}



\seealso{See examples in  \code{\link{Model}}, \code{\link{GeneralModel}}, \code{\link{GeneralModel_14}}, \code{\link{TwopParallelModel}}, 
\code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}}, etc.}

