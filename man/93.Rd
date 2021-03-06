\name{TimeMap.new}
\alias{TimeMap.new}
\title{basic constructor of the class TimeMap.}
\description{A TimeMap is nothing more than an usual R-function of one argument augmented by the lower and upper boundary of the interval where it is defined.}
\usage{TimeMap.new(t_start, t_end, f)}\arguments{
  \item{t_start}{A number marking the begin of the time domain where the function is valid}
  \item{t_end}{A number the end of the time domain where the function is valid}
  \item{f}{The time dependent function definition (a function in R's sense)}
}

\value{An object of class TimeMap that can be used to describe models.}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}





