\name{BoundFc-class}
\Rdversion{1.1}
\docType{class}
\alias{BoundFc-class}
\title{Class \code{"BoundFc"}}
\description{
Objects of this class contain a time dependent function describing the Atmospheric \eqn{^{14}C}{14C} fraction and a format description, that allows to use the numeric valuest to be interpreted correctly in subsequent computations.
}
\section{Slots}{
  \describe{
    \item{\code{starttime}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{endtime}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{map}:}{Object of class \code{"function"} ~~ }
    \item{\code{lag}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{format}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{AbsoluteFractionModern}{\code{signature(F = "BoundFc")}: ... }
    \item{Delta14C}{\code{signature(F = "BoundFc")}: ... }
    \item{getFormat}{\code{signature(object = "BoundFc")}: ... }
    \item{getFunctionDefinition}{\code{signature(object = "BoundFc")}: ... }
    \item{getTimeRange}{\code{signature(object = "BoundFc")}: ... }
    \item{initialize}{\code{signature(.Object = "BoundFc")}: ... }
	 }
}
\author{
 Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de> 
}
\examples{
showClass("BoundFc")
}
\keyword{classes}
