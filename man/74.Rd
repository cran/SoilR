\name{Model_14-class}
\Rdversion{1.1}
\docType{class}
\alias{Model_14-class}
\title{Class \code{"Model_14"}}
\description{
defines a representation of a 14C model
}
\section{Slots}{
  \describe{
    \item{\code{c14Fraction}:}{Object of class \code{"BoundFc"} ~~ }
    \item{\code{c14DecayRate}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{initialValF}:}{Object of class \code{"ConstFc"} ~~ }
    \item{\code{times}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{mat}:}{Object of class \code{"DecompOp"} ~~ }
    \item{\code{initialValues}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{inputFluxes}:}{Object of class \code{"InFlux"} ~~ }
    \item{\code{solverfunc}:}{Object of class \code{"function"} ~~ }
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
    \item{initialize}{\code{signature(.Object = "Model_14")}: ... }
	 }
}
\author{
 Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de> 
}
\examples{
showClass("Model_14")
}
\keyword{classes}
