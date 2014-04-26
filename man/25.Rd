\name{BoundLinDecompOp-methods}
\docType{methods}
\alias{BoundLinDecompOp-methods}
\alias{BoundLinDecompOp,ConstLinDecompOp,ANY,ANY,ANY-method}
\alias{BoundLinDecompOp,TimeMap,missing,missing,missing-method}
\alias{BoundLinDecompOp,function,numeric,numeric,missing-method}
\alias{BoundLinDecompOp,function,numeric,numeric,numeric-method}
\title{ ~~ Methods for Function \code{BoundLinDecompOp}  ~~}
\description{
 ~~ Methods for function \code{BoundLinDecompOp} ~~
}
\section{Methods}{
  \describe{
    \item{\code{signature(map = "ConstLinDecompOp", starttime = "ConstLinDecompOp", endtime = "ConstLinDecompOp", lag = "ConstLinDecompOp")}}{
      \code{\link{BoundLinDecompOp_method__ConstLinDecompOp}}  
    }
    \item{\code{signature(map = "TimeMap", starttime = "missing", endtime = "missing", lag = "missing")}}{
      \code{\link{BoundLinDecompOp_method__TimeMap_missing_missing_missing}}  
    }
    \item{\code{signature(map = "function", starttime = "numeric", endtime = "numeric", lag = "missing")}}{
      \code{\link{BoundLinDecompOp_method__function_numeric_numeric_missing}}  
    }
    \item{\code{signature(map = "function", starttime = "numeric", endtime = "numeric", lag = "numeric")}}{
      \code{\link{BoundLinDecompOp_method__function_numeric_numeric_numeric}}  
    }

  }
}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
