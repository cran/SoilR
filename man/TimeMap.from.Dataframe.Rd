\name{TimeMap.from.Dataframe}
\alias{TimeMap.from.Dataframe}
\title{TimeMap from Dataframe}
\description{This function is another constructor of the class TimeMap.}
\usage{TimeMap.from.Dataframe(dframe, interpolation = splinefun)}
\arguments{
  \item{dframe}{A data frame containing exactly two columns:
the first one is interpreted as time}
  \item{interpolation}{A function that  returns a function  the default is splinefun. Other possible values are the linear interpolation approxfun or any self made function with the same interface.}
}

\value{An object of class TimeMap that contains the interpolation function and the limits of the time range where the function is valid.
this serves as a saveguard for Model which thus can check that all involved functions of time are actually defined for the times of interest  }

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}





