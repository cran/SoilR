\name{Model-class}
\Rdversion{1.1}
\docType{class}
\alias{Model-class}
\alias{Model}
\alias{getC,Model-method}
\alias{getRelease,Model-method}
\alias{getReleaseFlux,Model-method}
\alias{getTimes,Model-method}
\alias{initialize,Model-method}

\title{Model}
\description{serves as a fence to the interface of SoilR functions. So that later implementations can differ	 }
\section{Objects from the Class}{Objects can be created by calls of the form \code{new(Model ...)}}
\section{Slots}{
  \describe{
    \item{\code{times}:}{Object of class \code{"numeric"}}
    \item{\code{mat}:}{Object of class \code{"TimeMap"}}
    \item{\code{initialValues}:}{Object of class \code{"numeric"}}
    \item{\code{inputFluxes}:}{Object of class \code{"TimeMap"}}
    \item{\code{solverfunc}:}{Object of class \code{"function"}}
  }
}
\section{Methods}{
  \describe{
    \item{getC}{\code{signature(object = "Model")}: ... }
    \item{getRelease}{\code{signature(object = "Model")}: ... }
    \item{getReleaseFlux}{\code{signature(object = "Model")}: ... }
    \item{getTimes}{\code{signature(object = "Model")}: ... }
    \item{initialize}{\code{signature(.Object = "Model")}: ... }
	 }
}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}





