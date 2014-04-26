\name{BoundInFlux_method__data.frame_missing_missing_numeric_function}
\alias{BoundInFlux_method__data.frame_missing_missing_numeric_function}
\title{constructor}
\description{This function is another constructor of the class BoundInFlux.}
\arguments{
  \item{map}{A data frame; the first column is interpreted as time}
  \item{lag}{lag time}
  \item{interpolation}{function used for interpolation}
}

\value{An object of class BoundInFlux that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}





