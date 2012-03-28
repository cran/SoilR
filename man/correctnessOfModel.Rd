\name{correctnessOfModel}
\alias{correctnessOfModel}
\title{correctnessOfModel}
\description{The parameters used by the function \code{\link{GeneralModel}} in SoilR have a biological meaning, and therefore cannot be arbitrary.
This functions tests some of the obvious constraints of the general model. 
Up to now these are:
1) The compatibility of the decomposition rates and the transport parameters to and from other pools, i.e. 
the column-wise sum of the elements cannot be negative. Otherwise this would create negative values of respiration, which are not biologically meaningful.
2) The compatibility of the time ranges of the supplied functions }
\usage{correctnessOfModel(times, Atm, InputFluxes)}
\arguments{
  \item{times}{A vector containing the points in time where the solution is sought.}
  \item{Atm}{An object of class TimeMap containing a matrix valued function and the time range where it is applicable. The matrix function contains the whole model decomposition rates , connection and feedback coefficients for the n pools. The size of this matrix is equal to the number of pools.}
  \item{InputFluxes}{An object of class TimeMap containing a vecotor valued function and the time range where it is applicable. The vector function contains the whole model inputFluxes for the n pools. The size of this vector is therefor equal to the number of pools.}
}



\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}





