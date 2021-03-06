\name{getTransitTimeDistributionDensity_method__ConstLinDecompOp}
\alias{getTransitTimeDistributionDensity_method__ConstLinDecompOp}
\title{compute the TransitTimeDistributionDensity }
\description{This mehtod computes the probability density of the transit time of the linear time invariant system
that can be constructed from the given operator and input distribution.}
\arguments{
  \item{object}{
}
  \item{inputDistribution}{
}
  \item{times}{
}
}
\details{In a forthcoming paper \cite{SoilRv1.2} 
we derive the algorithm used in this implementation
under the assumption of steady conditions having prevailed infinitely.
We arrive at a formulation well known from the literature about 
time invariant linlear systems, cited e.g. in \cite{ManzoniJGR}.\cr
The somehow amazing result is that the weight of the transit time density \eqn{\psi(T)}{\psi(T)} 
for a \emph{transit time} \eqn{T}{T} 
for the steady state system is identical to the output \eqn{O(T)}{O(T)} 
observed at time \eqn{T}{T} of a \emph{different} system which started with a normalized impulsive input 
\eqn{\frac{\vec{I}}{I}}{\frac{\vec{I}}{I}} at time 
\eqn{T=0}{T=0},
where \eqn{I=\sum_{k=1}^m i_k} is the cumulative input flux to all pools.
\cr
This fact simpliefies the computation considerably.
Translated into the language of an ode solver an impulsive input becomes a start vector \eqn{\frac{\vec{I}}{I}}{\frac{\vec{I}}{I}} 
at time \eqn{T=0}{T=0} 
and \eqn{O(T)}{O(T)} the respiration related to the solution of the initial value problem 
observed at time \eqn{T}{T}. 
\deqn{
\psi(T)=S_r \left( \frac{\vec{I}}{I},0,T\right)
}{}
Note that from the perspective of the ode solver \eqn{S_r}{S_r} depends on the decomposition operator and the distribution of the input among the pools only.
It is therefor possible to implement the transit time distribution as a function of the decomposition operator and the fixed input flux distribution.
To insure steady state conditions the decomposition operator is not allowed to be a true function of time.
We therefor implement the method only for the subclass 
\code{ConstLinDecompOp} 
\cr Remark:\cr
The decision to implement this method for \code{transitTimeDensity} especially for 
objects of class \code{ConstLinDecompOp}
reflects the fact that an arbitrary  model in SoilR is by no means bound to end up in steady state. To insure this we would have to ignore the input part of a user created model which would be confusing. 
\cr Remark:\cr
In future versions of SoilR it will be possible to compute a dynamic, time dependent transit time distribution 
for objects of class \code{ Model}
with a time argument specifying for which time the distribution is sought. 
The steady state results computed here could than be reproduced 
with the user responsible for providing a model actually reaching it. }

\references{Manzoni, S., Katul, G.~G., and Porporato, A.: Analysis of soil carbon transit
times and age distributions using network theories, J. Geophys. Res., 114,
}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}





