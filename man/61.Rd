\name{GeneralModel_method__numeric_ANY_numeric}
\alias{GeneralModel_method__numeric_ANY_numeric}
\title{a constructor for class Model}
\description{This method tries to create a Model object from any combination of arguments 
that can be converted into  the required set of building blocks for a model
for n arbitrarily connected pools.}
\arguments{
  \item{t}{A vector containing the points in time where the solution is sought.}
  \item{A}{something that can be converted to any of the available DecompositionOperator classes}
  \item{ivList}{A vector containing the initial amount of carbon for the n pools. The length of this vector is equal to the number of pools and thus equal to the length of k. This is checked by an internal  function.}
  \item{inputFluxes}{something that can be converted to any of the available InFlux classes}
  \item{solverfunc}{The function used by to actually solve the ODE system. This can be \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.}
  \item{pass}{Forces the constructor to create the model even if it is invalid}
}

\value{A model object that can be further queried. }

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}



\seealso{\code{\link{TwopParallelModel}}, \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}} }

