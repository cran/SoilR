\name{FcAtm.from.Dataframe}
\alias{FcAtm.from.Dataframe}
\title{FcAtm from Dataframe}
\description{This function is another constructor of the class FcAtm}
\usage{FcAtm.from.Dataframe(dframe, lag = 0, interpolation = splinefun, 
    format)}
\arguments{
  \item{dframe}{A data frame containing exactly two columns:
the first one is interpreted as time
the secon one is interpreted as atmospheric C14 fraction in the format mentioned}
  \item{lag}{a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)}
  \item{interpolation}{A function that  returns a function  the default is splinefun. Other possible values are the linear interpolation approxfun or any self made function with the same interface.}
  \item{format}{a string that specifies the format used to represent the atmospheric fracton. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation}
}

\value{An object of class TimeMap that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}





