\name{GeneralModel_14}
\alias{GeneralModel_14}
\title{The most general costructor for class Model14}
\description{The function creates a numerical model  for n arbitrarily connected pools.
It is one of the constructors of class Model14 which is a subclass of Model
It will be used by some more specialized wrapper functions like for instance but can also be used directly. It is in fact the most
versatile interface to produce instances of class Model14. }
\usage{GeneralModel_14(t, A, ivList, inputFluxes, Fc, di = -0.0001209681, 
    solverfunc = deSolve.lsoda.wrapper, pass = FALSE)}
\arguments{
  \item{t}{A vector containing the points in time where the solution is sought.}
  \item{A}{A DecompositionOperator object consisting of  a matrix valued function describing the whole model decay rates for the n pools, connection and feedback coefficients as functions of time and a time range for which this function is valid. The size of the quadtratic matric must be equal to the number of pools. The time range must cover the times given in the first argument.}
  \item{ivList}{A vector containing the initial amount of carbon for the n pools. The length of this vector is equal to the number of pools and thus equal to the length of k. This is checked by the function \code{\link{correctnessOfModel}}.}
  \item{inputFluxes}{A TimeMap object consisting of a vector valued function describing the inputs to the pools as funtions of time \code{\link{TimeMap.new}}.}
  \item{Fc}{A TimeMap object consisting of  a function describing the fraction of C_14 in per mille.}
  \item{di}{
}
  \item{solverfunc}{The function used by to actually solve the ODE system. This can be \code{\link{SoilR.euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.}
  \item{pass}{if TRUE Forces the constructor to create the model even if it is invalid}
}

\value{A model object that can be further queried. }

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}



\seealso{\code{\link{Model}} }
\examples{
t_start=1960
t_end=2010
tn=220
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
n=3
At=new(Class="DecompositionOperator",
  t_start,
  t_end,
  function(t0){
        matrix(nrow=n,ncol=n,byrow=TRUE,
          c(-1,    0.1,    0, 
             0.5  , -0.4,    0,   
             0,    0.2,   -0.1)
        )
  }
) 
 
c0=c(100, 100, 100)
#constant inputrate
inputFluxes=new(
  "TimeMap",
  t_start,
  t_end,
  function(t0){matrix(nrow=n,ncol=1,c(10,10,10))}
) 
# we have a dataframe representing the C_14 fraction 
# note that the time unit is in years.
# This means that all the other data provided are assumed to have the same value
# This is especially true for the decay constants to be specified later
data(C14Atm_NH)
Fc=TimeMap.from.Dataframe(C14Atm_NH)
#Fc=TimeMap.from.Dataframe(C14Atm_NH)
# add the C14 decay to the matrix which is done by a diagonal matrix which does not vary over time
# we assume a half life th=5730 years
th=5730
k=log(0.5)/th #note that k is negative and has the unit y^-1

mod=GeneralModel_14(t,At,c0,inputFluxes,Fc,k)
#start plots
par(mfrow=c(3,2))
   lt1=1;  lt2=2; lt3=3 
   col1=1;  col2=2; col3=3
   # plot the C and C14 curves
   Ct=getC(mod)
   plot(t,Ct[,1],type="l",lty=lt1,col=col1, ylim=c(0,200),
        ylab="C stocks (arbitrary units)",xlab="Time") 
   lines(t,Ct[,2],type="l",lty=lt2,col=col2) 
   lines(t,Ct[,3],type="l",lty=lt3,col=col3,lwd=2) 
   legend(
      "topright",
      c("C in pool 1",
        "C in pool 2",
        "C in pool 3"
      ),
      lty=c(lt1,lt2,lt3),
      col=c(col1,col2,col3)
   )
   C14t=getC14(mod)
   plot(t,C14t[,1]/1000,type="l",lty=lt1,col=col1, ylim=c(0,100),
        ylab="14C stocks (arbitrary units)",xlab="Time") 
   lines(t,C14t[,2]/1000,type="l",lty=lt2,col=col2) 
   lines(t,C14t[,3]/1000,type="l",lty=lt3,col=col3) 
   legend(
      "topright",
      c("14C in pool 1",
        "14C in pool 2",
        "14C in pool 3"
      ),
      lty=c(lt1,lt2,lt3),
      col=c(col1,col2,col3)
   )
   #now plot the C14 Fraction in the atmosphere and compute the C14/C fraction of in the Soil 

   FC14=getSoilC14Fraction(mod)
   plot(C14Atm_NH, type="l",xlim=c(1960,2010))
   lines(t,FC14[,1],lty=lt1,col=col1) 
   lines(t,FC14[,2],lt2,type="l",lty=lt2,col=col2) 
   lines(t,FC14[,3],type="l",lty=lt3,col=col3) 
   legend("topleft",c(
                      expression(F[1]),
                      expression(F[2]),
                      expression(F[3])
                    )
   ,lty=c(lt1,lt2,lt3),col=c(col1,col2,col3))

    
#now compute the release flux
   Rt=getReleaseFlux(mod)
   plot(t,Rt[,1],type="l",lty=lt1,col=col1,ylab="C Release Flux (arbitrary units)",xlab="Time",ylim=c(0,50)) 
   lines(t,Rt[,2],lt2,type="l",lty=lt2,col=col2) 
   lines(t,Rt[,3],type="l",lty=lt3,col=col3) 
   legend("topleft",c("RF1","RF2","RF3"),lty=c(lt1,lt2,lt3),col=c(col1,col2,col3))
   #now compute the c14 release flux
   R14t=getReleaseFlux14(mod)/1000
   plot(t,R14t[,1],type="l",lty=lt1,col=col1,ylab="C14 Release Flux (arbitrary units)",xlab="Time") 
   lines(t,R14t[,2],lt2,type="l",lty=lt2,col=col2) 
   lines(t,R14t[,3],type="l",lty=lt3,col=col3) 
   legend("topleft",c(
                      expression(RF[14]^1),
                      expression(RF[14]^2),
                      expression(RF[14]^3)
                    )
   ,lty=c(lt1,lt2,lt3),col=c(col1,col2,col3))

R14m=getTotalReleaseFluxC14CRatio(mod)
C14m=getTotalC14CRatio(mod)
plot(C14Atm_NH, type="l",xlim=c(1960,2010),col=4)
lines(t,C14m) 
lines(t,R14m,col=2) 
legend("topright",c("Atmosphere","Mean SOM-14C","Mean Release 14C"),lty=rep(1,3),col=c(4,1,2),bty="n")

par(mfrow=c(1,1))

}
