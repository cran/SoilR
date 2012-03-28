\name{getTransitTimeDistributionDensity}
\alias{getTransitTimeDistributionDensity}
\title{access to the transit time distribution }
\description{This function computes the transit time distribution density  for a given Decomposition Operator
by computing the output (respiration) of the system according to an instantaneous input.}
\usage{getTransitTimeDistributionDensity(object, inputDistribution, 
    times)}
\arguments{
  \item{object}{
}
  \item{inputDistribution}{to one!.}
  \item{times}{
}
}
\details{Let \eqn{O(t)} describe the overall output  flux of the system.
One can imagine  \eqn{O(t)} as a weighted sum of previous inputs \eqn{I(t-T)}. The weights for 
this summation are
described by the transit time distribution density \eqn{\psi(T)} where \eqn{T} is the 
transit time, the time from entry to exit of a particle.
This is expressed by the following relation:
\eqn{O(t)=\int_0^\infty \psi(T) I(t-T) dT}
For an instantaneous input \eqn{I(t-T)=\delta(t-T)} the transit time distribution density becomes
equal to the output of the system, the respiration rate. 
we can therefore compute the transfer time distribution by applying our Decomposition Operator to an 
instantaneous imput, which is realized by an inputrate of zero and startvalues of one.}

\references{1.) <ManzoniJGR>}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}





