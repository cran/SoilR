\name{fW.Skopp}
\alias{fW.Skopp}
\title{Effects of moisture on decomposition rates according to the function proposed by Skopp et al. 1990}
\description{Calculates the effects of relative soil water content on decomposition rates.}
\usage{fW.Skopp(rwc, alpha = 2, beta = 2, f = 1.3, g = 0.8)}
\arguments{
  \item{rwc}{relative water content}
  \item{alpha}{Empirical parameter}
  \item{beta}{Empirical parameter}
  \item{f}{Empirical parameter}
  \item{g}{Empirical parameter}
}


\references{J. Skopp, M. D. Jawson, and J. W. Doran. 1990.
Steady-state aerobic microbial activity as a function of soil water content. Soil Sci. Soc. Am. J., 54(6):1619-1625}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}




\examples{
th=seq(0,1,0.01)
xi=fW.Skopp(rwc=th)
plot(th,xi,type="l",main="Effects of soil water content on decomposition rates",
     xlab="Relative water content",ylab=expression(xi))
}
