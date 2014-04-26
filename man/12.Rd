\name{BoundFc_method__data.frame_missing_missing_numeric_character_function}
\alias{BoundFc_method__data.frame_missing_missing_numeric_character_function}
\title{constructor }
\description{the method constructs an object from a dataframe a timelag format using the given interpolating function}
\arguments{
  \item{map}{A data frame containing exactly two columns:
the first one is interpreted as time
the second one is interpreted as atmospheric C14 fraction in the format mentioned}
  \item{lag}{a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)}
  \item{format}{a string that specifies the format used to represent the atmospheric fraction. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation}
  \item{interpolation}{A function that  returns a function  the default is splinefun. Other possible values are the linear interpolation approxfun or any self made function with the same interface.}
}

\value{An object  that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}





