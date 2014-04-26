\name{getReleaseFlux14_method__Model_14}
\alias{getReleaseFlux14_method__Model_14}
\title{14C respiration rate for all pools }
\description{
The function computes the \eqn{^{14}C}{14C} release flux ( mass per time ) for all pools.
Note that the respiration coefficients for \eqn{^{14}C}{14C} 
do not change in comparison to the total C case.
The fraction of \eqn{^{14}C}{14C} lost by respiration 
is not greater for \eqn{^{14}C}{14C} 
although the decay is faster due to the contribution of radioactivity.}
\arguments{
  \item{object}{an object}
}



\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{
years=seq(1901,2009,by=0.5)
LitterInput=700 

Ex=TwopParallelModel14(
	t=years,
	ks=c(k1=1/2.8, k2=1/35),
	C0=c(200,5000), 
        F0_Delta14C=c(0,0),
	In=LitterInput, 
	gam=0.7,
	inputFc=C14Atm_NH,
	lag=2
)

R14=getReleaseFlux14(Ex)
C14=getC14(Ex)

par(mfrow=c(2,1))
 plot(years, C14[,1], col=4,type="l",xlab="Year",ylab="14C(t)",ylim=c(min(C14),max(C14))) 
 lines(years, C14[,2],col=4,lwd=2)
 legend("topright",c( "14C pool 1", "14C pool 2"),
       lty=c(1,1),col=c(1,4),lwd=c(1,1),bty="n")

 plot(years, R14[,1], col=4,type="l",xlab="Year",ylab="14R(t)",ylim=c(min(R14),max(R14))) 
 lines(years, R14[,2],col=4,lwd=2)
 legend("topright",c( "14C respiration pool 1", "14C respiration pool 2"),
       lty=c(1,1),col=c(1,4),lwd=c(1,1),bty="n")
par(mfrow=c(1,1))}
