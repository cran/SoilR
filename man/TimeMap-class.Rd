\name{TimeMap-class}
\Rdversion{1.1}
\docType{class}
\alias{TimeMap-class}
\alias{TimeMap}
%% \alias{as.character}
%% \alias{getFunctionDefinition}
%% \alias{getTimeRange}
%% \alias{initialize}

\title{TimeMap S4 class}
\description{defines a (time dependent) mapping including the function definition and the ### domain where the function is well define.  This can be used to avoid interpolations out of range when mixing different time dependent data sets}
\section{Objects from the Class}{Objects can be created by calls of the form \code{new(TimeMap ...)}}
\section{Slots}{
  \describe{
    \item{\code{starttime}:}{Object of class \code{"numeric"}}
    \item{\code{endtime}:}{Object of class \code{"numeric"}}
    \item{\code{map}:}{Object of class \code{"function"}}
    \item{\code{lag}:}{Object of class \code{"numeric"}}
  }
}
\section{Methods}{
  \describe{
    \item{as.character}{\code{signature(x = "TimeMap")}: ... }
    \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... }
    \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... }
	 }
}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}





