\name{Model_14-class}
\Rdversion{1.1}
\docType{class}
\alias{Model_14-class}
\alias{Model_14}
%% \alias{getC14}
%% \alias{getF14}
%% \alias{getF14C}
%% \alias{getF14R}
%% \alias{getReleaseFlux14}
%% \alias{initialize}

\title{Model_14}
\description{defines a representation of a 14C model}
\section{Objects from the Class}{Objects can be created by calls of the form \code{new(Model_14 ...)}}
\section{Slots}{
  \describe{
    \item{\code{c14Fraction}:}{Object of class \code{"TimeMap"}}
    \item{\code{c14DecayRate}:}{Object of class \code{"numeric"}}
    \item{\code{initialValF}:}{Object of class \code{"SoilR.F0"}}
    \item{\code{times}:}{Object of class \code{"numeric"}}
    \item{\code{mat}:}{Object of class \code{"TimeMap"}}
    \item{\code{initialValues}:}{Object of class \code{"numeric"}}
    \item{\code{inputFluxes}:}{Object of class \code{"TimeMap"}}
    \item{\code{solverfunc}:}{Object of class \code{"function"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{Model}"}, directly.
}
\section{Methods}{
  \describe{
    \item{getC14}{\code{signature(object = "Model_14")}: ... }
    \item{getF14}{\code{signature(object = "Model_14")}: ... }
    \item{getF14C}{\code{signature(object = "Model_14")}: ... }
    \item{getF14R}{\code{signature(object = "Model_14")}: ... }
    \item{getReleaseFlux14}{\code{signature(object = "Model_14")}: ... }
	 }
}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}





