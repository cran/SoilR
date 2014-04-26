\name{plotC14Pool}
\alias{plotC14Pool}
\title{Plots the output of \code{\link{getC14}} for each pool over time}
\description{This function produces a plot with the Delta14C in the atmosphere and the Delta14C of each pool obtained after a call to \code{\link{getC14}}.}
\usage{plotC14Pool(t, mat, inputFc, col, ...)}\arguments{
  \item{t}{A vector containing the time points for plotting.}
  \item{mat}{A matrix object obtained after a call to \code{\link{getC14}}}
  \item{inputFc}{A Data Frame object containing values of atmospheric Delta14C per time. First column must be time values, second column must be Delta14C values in per mil.}
  \item{col}{A color palette specifying color lines for each pool (columns of \code{mat}).}
  \item{\dots}{Other arguments passed to \code{plot}.}
}



\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{
years=seq(1901,2009,by=0.5)
LitterInput=700 

Ex=ThreepFeedbackModel14(
  t=years,ks=c(k1=1/2.8, k2=1/35, k3=1/100),
  C0=c(200,5000,500), F0=c(0,0,0),
  In=LitterInput, a21=0.1,a12=0.01,a32=0.005,a23=0.001,inputFc=C14Atm_NH
)
C14t=getF14(Ex)

pal=rainbow(3)
plotC14Pool(
  t=years,mat=C14t,inputFc=C14Atm_NH,
  col=pal,xlab="Time (yrs)",
  ylab="Delta 14C (per mil)",
  xlim=c(1950,2000)
)
}
