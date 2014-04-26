\name{getReleaseFlux_method__Model}
\alias{getReleaseFlux_method__Model}
\title{get the release rate for all pools }
\description{The method computes the release of carbon per time for all points in time 
specified in the Model objects time slot.}
\arguments{
  \item{object}{an object of class Model
created by a call to a constructor e.g. \code{\link{Model}}, 
\code{\link{GeneralModel}}or other model creating functions.}
}
\details{This function takes a Model object, which represents a system of ODEs 
\deqn{\frac{d \mathbf{C}(t)}{dt} = \mathbf{I}(t) + \mathbf{A}(t) \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t)} 
solves the system for \eqn{\mathbf{C}(t)}{C(t)}, calculates the release coefficients \eqn{\mathbf{R}(t)}{R(t)}, 
and computes the release flux as \eqn{\mathbf{R}(t) \mathbf{C}(t)}{R(t) C(t)}.
The numerical solver used can be specified in the model creating functions like e.g. \code{\link{Model}}.}
\value{A n x m matrix of release fluxes with m columns representing the number of pools, and n rows representing the time step as specified by the argument
\code{t} in \code{\link{Model}}, \code{\link{GeneralModel}} or another model creating function.}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{# create the model
t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 

Ex=TwopParallelModel(t,ks=c(k1=0.5,k2=0.2),C0=c(c10=100, c20=150),In=10,gam=0.7,xi=0.5)
# get the carbon stocks 
Ct=getC(Ex)
# get the release rates
Rt=getReleaseFlux(Ex)

par(mfrow=c(2,1))
plot(t,rowSums(Ct),type="l",lwd=2,
     ylab="Carbon stocks (arbitrary units)",xlab="Time",ylim=c(0,sum(Ct[1,]))) 
lines(t,Ct[,1],col=2)
lines(t,Ct[,2],col=4)
legend("topright",c("Total C","C in pool 1", "C in pool 2"),
       lty=c(1,1,1),col=c(1,2,4),lwd=c(2,1,1),bty="n")

plot(t,rowSums(Rt),type="l",ylab="Carbon released (arbitrary units)",
     xlab="Time",lwd=2,ylim=c(0,sum(Rt[1,]))) 
lines(t,Rt[,1],col=2)
lines(t,Rt[,2],col=4) 
legend("topright",c("Total C release","C release from pool 1", "C release from pool 2"),
       lty=c(1,1,1),col=c(1,2,4),lwd=c(2,1,1),bty="n")

par(mfrow=c(1,1))}
