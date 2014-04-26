\name{getMeanTransitTime}
\alias{getMeanTransitTime}
\title{Access to the mean transit time}
\description{This generic function assembles methods to 
compute mean transit times.
The nature of the results can change considerably 
depending on the arguments of the function.
For an argument of class \code{model} 
it means something different than for an object of class 
\code{DecompOp}
To interpret them correctly refer also to the documentation
of the methods.}
\usage{getMeanTransitTime(object, inputDistribution)}\arguments{
  \item{object}{a DecompOp Object.}
  \item{inputDistribution}{a vector of length equal to the number of pools. The entries are weights, which must sum to 1.}
}
\details{The concept of mean transit time also known as mean 
residence time can be used to describe 
compartment models. 
In particular it is very frequently used to characterize
linear time invariant compartment models in steady state.  \cite{1, 2, 3, 4}. 
It is very important to note that SoilR is \emph{not} limited to those. 
To integrate the concept into the more general context therefor requires some care. 
This starts with the definition.
Assuming a time invariant system in steady state described by the 
(constant) distribution of inputs to the pools and constant decomposition 
and transfer rates one can define  
the mean transit time as the average time a package of carbon 
spends in the system from entry to exit.
From SoilRs general perspective 
this defintion is ambigous with regard to several points.
\enumerate{
\item
It does not take into account that the mean transfer time may change 
itself with time. For time invariant models in steady state this does not 
matter since the mean transit time turns out to be time invariant also but 
for a general model in SoilR 
input fluxes and decomposition coefficients can be time dependent and the 
system as a whole far from steady state.
\item
It does not specify the set of particles contributing to the mean value. 
If the system is forever in a steady state it is possible to think of the 
average transit time of all particles but if the system changes with time 
such a definition would not be to usefull. To be able to compare 
time dependent  models with real measurements  
the set of particles leaving the system at a certain point in time is a more 
natural choice
}
To incorporate the concept of transit times into SoilR we need to 
address these ambiguities. We also would like the new 
definition to agree with the old one in the special but often studied 
case of linear systems in steady state.
We suggest the following Definition: \cr
Given a system described by 
the complete history of inputs \eqn{\mathbf{I}(t)}{\mathbf{I}(t)} 
for \eqn{t\in (t_{start},t_0)}{t\in (t_{start},t_0)} 
to all pools until time \eqn{t_0}{t_0} 
and the cumulative output \eqn{O(t_0)}{O(t_0)} 
of all pools at time \eqn{t_0}{t_0}
the mean transit time \eqn{\bar T_{t_0}}{\bar T_{t_0}} 
\bold{of the system}
\bold{at time} \eqn{t_0}{t_0} 
is the average of the transit times of all particles leaving the system at time \eqn{t_0}{t_0}
Remark:\cr
For a system with several output channels one could define the mean transit time of particles leaving by this specific channel.
Remark:\cr
In future versions of SoilR it will be possible to compute a dynamic, time dependent mean transit time 
for objects of class \code{ Model}
There is also a method that constructs a time invariant mean transit time by creting a time invariant model in steady state from an input flux distribution and a constant decompostion operators.
This emphasizes that different methods for this function really answer different questions.}

\references{Manzoni, S., G.G. Katul, and A. Porporato. 2009. Analysis of soil carbon transit times and age distributions using network theories.
Journal of Geophysical Research-Biogeosciences 114, DOI: 10.1029/2009JG001070.

Thompson, M.~V. and Randerson, J.~T.: Impulse response functions of terrestrial
carbon cycle models: method and application, Global Change Biology, 5,
371--394, 10.1046/j.1365-2486.1999.00235.x, 1999.

Bolin, B. and Rodhe, H.: A note on the concepts of age distribution and transit
time in natural reservoirs, Tellus, 25, 58--62, 1973.

Eriksson, E.: Compartment Models and Reservoir Theory, Annual Review of Ecology
and Systematics, 2, 67--84, 1971.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}





