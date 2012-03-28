\name{RothCModel}
\alias{RothCModel}
\title{Implementation of the RothCModel}
\description{This function implements the RothC model of Jenkinson et al. It is a wrapper for the more general function \code{\link{GeneralModel}}.}
\usage{RothCModel(t, ks = c(k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66, k.HUM = 0.02, 
    k.IOM = 0), C0 = c(0, 0, 0, 0, 2.7), In = 1.7, DR = 1.44, 
    clay = 23.4, xi = 1, solver = deSolve.lsoda.wrapper)}
\arguments{
  \item{t}{A vector containing the points in time where the solution is sought.}
  \item{ks}{A vector of lenght 5 containing the values of the decomposition rates for the different pools}
  \item{C0}{A vector of length 5 containing the initial amount of carbon for the 5 pools.}
  \item{In}{A scalar or data.frame object specifying the amount of litter inputs by time.}
  \item{DR}{A scalar representing the ratio of decomposable plant material to resistant plant material (DPM/RPM).}
  \item{clay}{Percent clay in mineral soil.}
  \item{xi}{A scalar or data.frame object specifying the external (environmental and/or edaphic) effects on decomposition rates.}
  \item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
}

\value{A Model Object that can be further queried }
\references{Jenkinson, D. S., S. P. S. Andrew, J. M. Lynch, M. J. Goss, and P. B. Tinker. 1990. The Turnover of Organic Carbon and Nitrogen in Soil. 
Philosophical Transactions: Biological Sciences 329:361-368.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}



\seealso{\code{\link{ICBMModel}} }
\examples{
 t=0:500 
 Ex=RothCModel(t)
 Ct=getC(Ex)
 Rt=getReleaseFlux(Ex)
 
plot(t,Ct[,1],type="l",col=1, ylim=c(0,25),ylab=expression(paste("Carbon stores (Mg C", ha^-1,")")),xlab="Time (years)",lwd=2) 
lines(t,Ct[,2],col=2,lwd=2,lty=2) 
lines(t,Ct[,3],col=3,lwd=2,lty=3)
lines(t,Ct[,4],col=4,lwd=2,lty=4)
lines(t,Ct[,5],col=5,lwd=2,lty=5)
lines(t,rowSums(Ct),lwd=2)
legend("topright",c("Pool 1, DPM", "Pool 2, RPM",  "Pool 3, BIO","Pool 4, HUM","Pool 5, IOM","Total Carbon"),lty=c(1:5,1),lwd=rep(2,5),col=c(1,2,3,4,5,"black"),bty="n")

plot(t,Rt[,1],type="l",ylim=c(0,2),ylab="Respiration (Mg C ha-1 yr-1)",xlab="Time") 
lines(t,Rt[,2],col=2) 
lines(t,Rt[,3],col=3) 
lines(t,Rt[,4],col=4) 
lines(t,Rt[,5],col=5) 
lines(t,rowSums(Rt),lwd=2) 
legend("topright",c("Pool 1, DPM", "Pool 2, RPM",  "Pool 3, BIO","Pool 4, HUM","Pool 5, IOM","Total Respiration"),lty=c(1,1,1,1,1,1),lwd=c(1,1,1,1,1,2),col=c(1,2,3,4,5,1),bty="n")

}
