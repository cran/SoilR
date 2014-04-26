\name{DecompositionOperator-class}
\Rdversion{1.1}
\docType{class}
\alias{DecompositionOperator-class}
\title{Class \code{"DecompositionOperator"}}
\description{
The new class implementing the same functionality is names \code{BoundLinDecompOp}
}
\section{Slots}{
  \describe{
    \item{\code{map}:}{Object of class \code{"function"} ~~ }
    \item{\code{lag}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{starttime}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{endtime}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{DecompOp}"}, directly.
}
\section{Methods}{
  \describe{
    \item{getFunctionDefinition}{\code{signature(object = "DecompositionOperator")}: ... }
    \item{getTimeRange}{\code{signature(object = "DecompositionOperator")}: ... }
    \item{initialize}{\code{signature(.Object = "DecompositionOperator")}: ... }
	 }
}
\author{
 Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de> 
}
\examples{
showClass("DecompositionOperator")
}
\keyword{classes}
