\name{ThreepParallelModel14}
\alias{ThreepParallelModel14}
\title{Implementation of a three-pool C14 model with parallel structure}
\description{This function creates a model for two independent (parallel) pools. 
It is a wrapper for the more general function \code{\link{GeneralModel_14}} that can handle an arbitrary number of pools.}
\usage{ThreepParallelModel14(t, ks, C0, In, gam1, gam2, xi = 1, FcAtm, 
    lambda = -0.0001209681, lag = 0, solver = deSolve.lsoda.wrapper)}
\arguments{
  \item{t}{A vector containing the points in time where the solution is sought. It must be specified within the same period for which the Delta 14 C of the atmosphere is provided. The default period in the provided dataset \code{\link{C14Atm_NH}} is 1900-2010.}
  \item{ks}{A vector of length 3 containing the decomposition rates for the 3 pools.}
  \item{C0}{A vector of length 3 containing the initial amount of carbon for the 3 pools.}
  \item{In}{A scalar or a data.frame object specifying the amount of litter inputs by time.}
  \item{gam1}{A scalar representing the partitioning coefficient, i.e. the proportion from the total amount of inputs that goes to pool 1.}
  \item{gam2}{A scalar representing the partitioning coefficient, i.e. the proportion from the total amount of inputs that goes to pool 2.}
  \item{xi}{A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates.}
  \item{FcAtm}{A Data Frame object consisting of  a function describing the fraction of C_14 in per mille.}
  \item{lambda}{Radioactive decay constant. By default lambda=-0.0001209681 y^-1 . This has the side effect that all your time related data are treated as if the time unit was year.}
  \item{lag}{A positive scalar representing a time lag for radiocarbon to enter the system.}
  \item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
}

\value{A Model Object that can be further queried }

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}



\seealso{\code{\link{TwopSeriesModel14}}, \code{\link{TwopFeedbackModel14}}  }
\examples{

data(C14Atm_NH)
#Fc=TimeMap.from.Dataframe(C14Atm_NH)
years=seq(1901,2009,by=0.5)
LitterInput=700 

Ex=ThreepParallelModel14(t=years,ks=c(k1=1/2.8, k2=1/35, k3=1/100),C0=c(200,5000,500), In=LitterInput, gam1=0.7, gam2=0.1, FcAtm=C14Atm_NH,lag=2)
R14m=getTotalReleaseFluxC14CRatio(Ex)
C14m=getTotalC14CRatio(Ex)
C14t=getSoilC14Fraction(Ex)

par(mfrow=c(2,1))
plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years, C14t[,1], col=4)
lines(years, C14t[,2],col=4,lwd=2)
lines(years, C14t[,3],col=4,lwd=3)
legend("topright",c("Delta 14C Atmosphere", "Delta 14C pool 1", "Delta 14C pool 2", "Delta 14C pool 3"),lty=rep(1,4),col=c(1,4,4,4),lwd=c(1,1,2,3),bty="n")

plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years,C14m,col=4)
lines(years,R14m,col=2)
legend("topright",c("Delta 14C Atmosphere","Delta 14C SOM", "Delta 14C Respired"),lty=c(1,1,1), col=c(1,4,2),bty="n")
par(mfrow=c(1,1))
}
