\name{GeneralModel_14}
\alias{GeneralModel_14}
\title{The most general costructor for class Model14}
\description{The function creates a numerical model  for n arbitrarily connected pools.
It is one of the constructors of class Model14 which is a subclass of Model
It will b used by some more specialized wrapper functions like for instance but can also be used directly. It is in fact the most
versatile interface to produce instances of class Model14. }
\usage{GeneralModel_14(t, A, ivList, inputFluxes, Fc, di = -0.0001209681, 
    solverfunc = deSolve.lsoda.wrapper)}
\arguments{
  \item{t}{A vector containing the points in time where the solution is sought.}
  \item{A}{A TimeMap object consisting of  a matrix valued function describing the whole model decay rates for the n pools, connection and feedback coefficients as functions of time and a time range for which this function is valid. The size of the quadtratic matric must be equal to the number of pools. The time range must cover the times given in the first argument.}
  \item{ivList}{A vector containing the initial amount of carbon for the n pools. The length of this vector is equal to the number of pools and thus equal to the length of k. This is checked by the function \code{\link{correctnessOfModel}}.}
  \item{inputFluxes}{A TimeMap object consisting of a vector valued function describing the inputs to the pools as funtions of time \code{\link{TimeMap.new}}.}
  \item{Fc}{A TimeMap object consisting of  a function describing the fraction of C_14 in per mille.}
  \item{di}{
}
  \item{solverfunc}{The function used by to actually solve the ODE system. This can be \code{\link{SoilR.euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.}
}

\value{A model object that can be further queried. }

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}



\seealso{\code{\link{TwopParallelModel}} }
\examples{
t_start=1900
t_end=2010
tn=220
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
n=3
At=TimeMap.new(
  t_start,
  t_end,
  function(t0){
        matrix(nrow=n,ncol=n,byrow=TRUE,
          c(-0.02,    0,    0, 
             0  , -0.03,    0,   
             0,      0,   -0.04)
        )
  }
) 
 
c0=c(0.5, 0.5, 0.5)
#constant inputrate
inputFluxes=TimeMap.new(
  t_start,
  t_end,
  function(t0){matrix(nrow=n,ncol=1,c(0.0,0,0))}
) 
# we have a dataframe representing the C_14 fraction 
# note that the time unit underlying it is years.
# This means that all the other data provided are assumed to have the same value
# This is especially true for the decay constants to be specified later
path=file.path(system.file(package="SoilR"),"data","C14Atm_NH.rda")
load(path)
Fc=TimeMap.from.Dataframe(C14Atm_NH)
# add the C14 decay to the matrix which is done by a diagonal matrix which does not vary over time
# we assume a half life of th=5730 years
th=5730
k=log(0.5)/th #note that k is negative and has the unit y^-1

mod=GeneralModel_14(t,At,c0,inputFluxes,Fc,k)
Y=getC(mod)
lt1=1;  lt2=2; lt3=3 
col1=1;  col2=2; col3=3
#x11()
plot(t,Y[,1],type="l",lty=lt1,col=col1,
     ylab="C stocks (arbitrary units)",xlab="Time") 
lines(t,Y[,2],type="l",lty=lt2,col=col2) 
lines(t,Y[,3],type="l",lty=lt3,col=col3) 
legend(
   "topright",
   c("C in pool 1",
     "C in pool 2",
     "C in pool 3"
   ),
   lty=c(lt1,lt2,lt3),
   col=c(col1,col2,col3)
)
#now compute the accumulated release
Y=getRelease(mod)
#x11()
plot(t,Y[,1],type="l",lty=lt1,col=col1,ylab="C Release (arbitrary units)",xlab="Time") 
lines(t,Y[,2],lt2,type="l",lty=lt2,col=col2) 
lines(t,Y[,3],type="l",lty=lt3,col=col3) 
legend("topleft",c("R1","R2","R3"),lty=c(lt1,lt2,lt3),col=c(col1,col2,col3))
Y=getRelease14(mod)
#x11()
plot(t,Y[,1],type="l",lty=lt1,col=col1,ylab="C Release (arbitrary units)",xlab="Time") 
lines(t,Y[,2],lt2,type="l",lty=lt2,col=col2) 
lines(t,Y[,3],type="l",lty=lt3,col=col3) 
legend("topleft",c(
                   expression(R[14]^1),
                   expression(R[14]^2),
                   expression(R[14]^3)
                 )
,lty=c(lt1,lt2,lt3),col=c(col1,col2,col3))
}
