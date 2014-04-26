\name{Model-class}
\Rdversion{1.1}
\docType{class}
\alias{Model-class}
\title{Class \code{"Model"}}
\description{
serves as a fence to the interface of SoilR functions. So that later implementations can differ	 
}
\section{Slots}{
  \describe{
    \item{\code{times}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{mat}:}{Object of class \code{"DecompOp"} ~~ }
    \item{\code{initialValues}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{inputFluxes}:}{Object of class \code{"InFlux"} ~~ }
    \item{\code{solverfunc}:}{Object of class \code{"function"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Model", i = "character")}: ... }
    \item{getAccumulatedRelease}{\code{signature(object = "Model")}: ... }
    \item{getC}{\code{signature(object = "Model")}: ... }
    \item{getReleaseFlux}{\code{signature(object = "Model")}: ... }
    \item{getTimes}{\code{signature(object = "Model")}: ... }
    \item{initialize}{\code{signature(.Object = "Model")}: ... }
    \item{plot}{\code{signature(x = "Model")}: ... }
    \item{print}{\code{signature(x = "Model")}: ... }
    \item{show}{\code{signature(object = "Model")}: ... }
    \item{summary}{\code{signature(object = "Model")}: ... }
	 }
}
\author{
 Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de> 
}
\examples{
showClass("Model")
}
\keyword{classes}
