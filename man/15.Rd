\name{BoundFc_method__function_numeric_numeric_numeric_character_missing}
\alias{BoundFc_method__function_numeric_numeric_numeric_character_missing}
\title{constructor}
\description{the method constructs an object from a function a timerange where it is valid and a format  }
\arguments{
  \item{map}{a function of one argument (time)}
  \item{starttime}{the point in time from which map is a valid representation}
  \item{endtime}{the point in time until which map is a valid representation}
  \item{lag}{a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)}
  \item{format}{a string that specifies the format used to represent the atmospheric fraction. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation}
}



\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}





