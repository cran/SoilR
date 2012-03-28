\name{DecompositionOperator-class}
\Rdversion{1.1}
\docType{class}
\alias{DecompositionOperator-class}
\alias{DecompositionOperator}
\alias{getMeanTransitTime,DecompositionOperator-method}
\alias{getTransitTimeDistributionDensity,DecompositionOperator-method}
\alias{initialize,DecompositionOperator-method}

\title{decomposition operator }
\description{#######################################################}
\section{Objects from the Class}{Objects can be created by calls of the form \code{new(DecompositionOperator ...)}}
\section{Slots}{
  \describe{
    \item{\code{starttime}:}{Object of class \code{"numeric"}}
    \item{\code{endtime}:}{Object of class \code{"numeric"}}
    \item{\code{map}:}{Object of class \code{"function"}}
    \item{\code{lag}:}{Object of class \code{"numeric"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{TimeMap}"}, directly.
}
\section{Methods}{
  \describe{
    \item{getMeanTransitTime}{\code{signature(object = "DecompositionOperator")}: ... }
    \item{getTransitTimeDistributionDensity}{\code{signature(object = "DecompositionOperator")}: ... }
    \item{initialize}{\code{signature(.Object = "DecompositionOperator")}: ... }
	 }
}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}





