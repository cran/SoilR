\name{TimeMap-class}
\Rdversion{1.1}
\docType{class}
\alias{TimeMap-class}
\alias{TimeMap}
\alias{getFunctionDefinition,TimeMap-method}
\alias{getTimeRange,TimeMap-method}
\alias{initialize,TimeMap-method}

\title{TimeMap S4 class}
\description{defines a (time dependent) mapping including the function definition and the ### domain where the function is well define.  This can be used to avoid interpolations out of range when mixing different time dependent data sets}
\section{Objects from the Class}{Objects can be created by calls of the form \code{new(TimeMap ...)}}
\section{Slots}{
  \describe{
    \item{\code{starttime}:}{Object of class \code{"numeric"}}
    \item{\code{endtime}:}{Object of class \code{"numeric"}}
    \item{\code{map}:}{Object of class \code{"function"}}
  }
}
\section{Methods}{
  \describe{
    \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... }
    \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... }
    \item{initialize}{\code{signature(.Object = "TimeMap")}: ... }
	 }
}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}





