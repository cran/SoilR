\name{BoundLinDecompOp-class}
\Rdversion{1.1}
\docType{class}
\alias{BoundLinDecompOp-class}
\title{Class \code{"BoundLinDecompOp"}}
\description{
NA
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
    \item{getFunctionDefinition}{\code{signature(object = "BoundLinDecompOp")}: ... }
    \item{getTimeRange}{\code{signature(object = "BoundLinDecompOp")}: ... }
    \item{initialize}{\code{signature(.Object = "BoundLinDecompOp")}: ... }
	 }
}
\author{
 Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de> 
}
\examples{
showClass("BoundLinDecompOp")
}
\keyword{classes}
