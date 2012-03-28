\name{correctnessOfModel}
\alias{correctnessOfModel}
\title{correctnessOfModel}
\description{The parameters used by the function \code{\link{GeneralModel}} in SoilR have a biological meaning, and therefore cannot be arbitrary.
This functions tests some of the obvious constraints of the general model. 
Up to now these are:
1) The compatibility of the decomposition rates and the transport parameters to and from other pools, i.e. 
the column-wise sum of the elements cannot be negative. Otherwise this would create negative values of respiration, which are not biologically meaningful.
2) The compatibility of the time ranges of the supplied functions }
\usage{correctnessOfModel(object)}
\arguments{
  \item{object}{
}
}



\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}





