\name{getC}
\alias{getC}
\title{Calculates the C content of the pools }
\description{This function computes the carbon content of the pools as function of time}
\usage{getC(object, as.closures = F)}
\arguments{
  \item{object}{An object of class Model or Model14 created by a call to \code{\link{GeneralModel}} or other model creating functions.}
  \item{as.closures}{if set to TRUE instead of a matrix a list of functions will be returned.}
}
\details{This function takes a Model object, which represents a system of ODEs of the form 
\deqn{\frac{d \mathbf{C}(t)}{dt} = \mathbf{I}(t) + \mathbf{A}(t) \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t)} 
and solves the system for \eqn{\mathbf{C}(t)}{C(t)}. The numerical solver used can be specified in \code{\link{GeneralModel}}.}
\value{A matrix with m columns representing the number of pools, and n rows representing the time step as specified by the argument
\code{t} in \code{\link{GeneralModel}} or other model creating function.}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}



\seealso{See examples in \code{\link{GeneralModel}}, \code{\link{GeneralModel_14}}, \code{\link{TwopParallelModel}}, 
\code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}}, etc.}

