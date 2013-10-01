\name{getReleaseFlux}
\alias{getReleaseFlux}
\title{Calculates the release of C from each pool}
\description{This function computes carbon release from each pool of the given model as funtion of time }
\usage{getReleaseFlux(object)}
\arguments{
  \item{object}{An object of class Model or Model14 created by a call to \code{\link{GeneralModel}} or other model creating functions.}
}
\details{This function takes a Model object, which represents a system of ODEs of the form 
\deqn{\frac{d \mathbf{C}(t)}{dt} = \mathbf{I}(t) + \mathbf{A}(t) \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t)} 
solves the system for \eqn{\mathbf{C}(t)}{C(t)}, calculates a diagonal matrix of release coefficients \eqn{\mathbf{R}(t)}{R(t)}, 
and computes the release flux as \eqn{\mathbf{R}(t) \mathbf{C}(t)}{R(t) C(t)}.
The numerical solver used can be specified in \code{\link{GeneralModel}}.}
\value{A n x m matrix of release fluxes with m columns representing the number of pools, and n rows representing the time step as specified by the argument
\code{t} in \code{\link{GeneralModel}} or other model creating function.}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}



\seealso{See examples in \code{\link{GeneralModel}}, \code{\link{GeneralModel_14}}, \code{\link{TwopParallelModel}}, 
\code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}}, etc.}

