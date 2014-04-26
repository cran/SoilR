\name{ConstLinDecompOp-class}
\Rdversion{1.1}
\docType{class}
\alias{ConstLinDecompOp-class}
\title{Class \code{"ConstLinDecompOp"}}
\description{
NA
}
\section{Slots}{
  \describe{
    \item{\code{mat}:}{Object of class \code{"matrix"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{DecompOp}"}, directly.
}
\section{Methods}{
  \describe{
    \item{BoundLinDecompOp}{\code{signature(map = "ConstLinDecompOp", starttime = "ANY", endtime = "ANY", lag = "ANY")}: ... }
    \item{getFunctionDefinition}{\code{signature(object = "ConstLinDecompOp")}: ... }
    \item{getMeanTransitTime}{\code{signature(object = "ConstLinDecompOp")}: ... }
    \item{getTimeRange}{\code{signature(object = "ConstLinDecompOp")}: ... }
    \item{getTransitTimeDistributionDensity}{\code{signature(object = "ConstLinDecompOp")}: ... }
    \item{initialize}{\code{signature(.Object = "ConstLinDecompOp")}: ... }
	 }
}
\author{
 Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de> 
}
\examples{
showClass("ConstLinDecompOp")
}
\keyword{classes}
