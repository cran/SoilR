\name{fW.Standcarb}
\alias{fW.Standcarb}
\title{Effects of moisture on decomposition rates according to the StandCarb model}
\description{Calculates the effects of moisture on decomposition rates according to the StandCarb model.}
\usage{fW.Standcarb(Moist, MatricShape = 5, MatricLag = 0, MoistMin = 30, 
    MoistMax = 350, DiffuseShape = 15, DiffuseLag = 4)}\arguments{
  \item{Moist}{A scalar or vector containing values of moisture content of a litter or soil pool (\%).}
  \item{MatricShape}{A scalar that determines when matric limit is reduced to the point that decay can begin to occur.}
  \item{MatricLag}{A scalar used to offset the curve to the left or right.}
  \item{MoistMin}{A scalar determining the minimum moisture content.}
  \item{MoistMax}{A scalar determining the maximum moisture content without diffusion limitations.}
  \item{DiffuseShape}{A scalar that determines the range of moisture contents where diffusion is not limiting.}
  \item{DiffuseLag}{A scalar used to shift the point when moisture begins to limit diffusion.}
}

\value{A data frame with limitation due to water potential (MatricLimit), limitation due to oxygen diffusion (DiffuseLimit), and the overall limitation of moisture on decomposition rates (MoistDecayIndex).}
\references{Harmon, M. E., and J. B. Domingo (2001), A users guide to STANDCARB version 2.0: 
A model to simulate carbon stores in forest stands. Oregon State University, Corvallis.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{
MC=0:500
DeadFoliage=fW.Standcarb(MC)
DeadBranch=fW.Standcarb(MC,MoistMax=200)
DeadWood=fW.Standcarb(MC,MoistMax=150)
StableSoil=fW.Standcarb(MC,MoistMin=15,MoistMax=100)
plot(MC,DeadFoliage$MoistDecayIndex,type="l",xlab="Moisture Content (\%)",
     ylab="f(W) (unitless)",
     main="Effects of moisture on decomposition rates according to the StandCarb model")
lines(MC,DeadBranch$MoistDecayIndex,col=4)
lines(MC,DeadWood$MoistDecayIndex,col=3)
lines(MC,StableSoil$MoistDecayIndex,col=2)
legend("topright",c("Dead Foliage","Dead Branch","Dead Wood","Stable Soil"),
       lty=c(1,1,1),col=c(1,4,3,2),bty="n")
}
