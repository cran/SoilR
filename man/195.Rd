\name{getTransitTimeDistributionDensity}
\alias{getTransitTimeDistributionDensity}
\title{methods for transit time distributions }
\description{According to  \link{getMeanTransitTime} to we define the related density:}
\usage{getTransitTimeDistributionDensity(object, inputDistribution, 
    times)}\arguments{
  \item{object}{a protoDecompOp Object}
  \item{inputDistribution}{a vector of length equal to the number of pools. The entries are weights. That means that their sume must be equal to one!}
  \item{times}{the times for which the distribution density is sought}
}
\details{Given a system described by
the complete history of  inputs \eqn{\mathbf{I}(t)}{\mathbf{I}(t)} 
for \eqn{t\in (t_{start},t_0)}{t\in (t_{start},t_0)} 
to all pools until time \eqn{t_0}{t_0} 
and
the cumulative output \eqn{O(t_0)}{O(t_0)} 
of all pools at time \eqn{t_0}{t_0}
the transit time density \eqn{\psi_{t_0}(T) }{\psi_{t_0}(T) }
\bold{of the system}
\bold{at time} \eqn{t_0}{t_0} is the probability density 
with respect to \eqn{T}{T} implicitly defined by
\deqn{\bar T_{t_0} = \int_0^{t-t_{start}} \psi_{t_0}(T) T \;dT}}

\references{Manzoni, S., G.G. Katul, and A. Porporato. 2009. Analysis of soil carbon transit times and age distributions using network theories.
Journal of Geophysical Research-Biogeosciences 114, DOI: 10.1029/2009JG001070.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}





