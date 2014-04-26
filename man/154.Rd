\name{getF14R_method__Model_14}
\alias{getF14R_method__Model_14}
\title{average radiocarbon fraction weighted by carbonrelease }
\description{Calculates the average radiocarbon fraction weighted by the amount of carbon release at each time step. 
\eqn{\overline{F_R}=\frac{\sum_{i=1}^{n}{^{14}R_i}}{\sum_{i=1}^{n}{R_i}}}{(14R_1(t)+...+14R_n(t)) )/(R_1(t)+...R_n(t)))}
Where \eqn{^{14}R_i(t)}{14R_i(t)} is the time dependent release of \eqn{^{14}C}{14C} of pool \eqn{i} and \eqn{R_i(t)}{R_i(t)} the release of all carbon isotops of pool \eqn{i}.   
Since the result is always in Absolute Fraction Modern format wie have to convert it to Delta14C}
\arguments{
  \item{object}{an object}
}

\value{A vector  of length n with the value of \eqn{\overline{F_R}}{FR} for each time step.}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{years=seq(1901,2009,by=0.5)
LitterInput=700 

Ex=TwopParallelModel14(t=years,ks=c(k1=1/2.8, k2=1/35),C0=c(200,5000), 
                       F0_Delta14C=c(0,0),In=LitterInput, gam=0.7,inputFc=C14Atm_NH,lag=2)
R14m=getF14R(Ex)
C14m=getF14C(Ex)
C14t=getF14(Ex)

par(mfrow=c(2,1))
plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years, C14t[,1], col=4)
lines(years, C14t[,2],col=4,lwd=2)
legend("topright",c("Delta 14C Atmosphere", "Delta 14C pool 1", "Delta 14C pool 2"),
       lty=c(1,1,1),col=c(1,4,4),lwd=c(1,1,2),bty="n")

plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years,C14m,col=4)
lines(years,R14m,col=2)
legend("topright",c("Delta 14C Atmosphere","Delta 14C SOM", "Delta 14C Respired"),
       lty=c(1,1,1), col=c(1,4,2),bty="n")
par(mfrow=c(1,1))}
