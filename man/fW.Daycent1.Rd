\name{fW.Daycent1}
\alias{fW.Daycent1}
\title{Effects of moisture on decomposition rates according to the DAYCENT model}
\description{Calculates the effects of Soil Water Content on decomposition rates according to the Daycent Model.}
\usage{fW.Daycent1(swc, a = 0.6, b = 1.27, c = 0.0012, d = 2.84, partd = 2.65, 
    bulkd = 1, width = 1)}
\arguments{
  \item{swc}{A scalar or vector with soil water content of a soil layer (cm).}
  \item{a}{Empirical coefficient. For fine textured soils a = 0.6. For coarse textured soils a = 0.55.}
  \item{b}{Empirical coefficient. For fine textured soils b = 1.27. For coarse textured soils b = 1.70.}
  \item{c}{Empirical coefficient. For fine textured soils c = 0.0012. For coarse textured soils c = -0.007.}
  \item{d}{Empirical coefficient. For fine textured soils d = 2.84. For coarse textured soils d = 3.22.}
  \item{partd}{Particle density of soil layer.}
  \item{bulkd}{Bulk density of soil layer (g/cm^3).}
  \item{width}{Thickness of a soil layer (cm).}
}

\value{A data frame with values of water filled pore space (wfps) and effects of soil water content on decomposition rates. Both vectors are unitless. }
\references{Kelly, R. H., W. J. Parton, M. D. Hartman, L. K. Stretch, D. S. Ojima, and D. S. Schimel (2000), 
Intra-annual and interannual variability of ecosystem processes in shortgrass steppe, J. Geophys. Res., 105.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}




\examples{
swc=seq(0,0.8,by=0.01)  # A sequence of values of soil water content
fine=fW.Daycent1(swc)
coarse=fW.Daycent1(swc,a=0.55,b=1.7,c=-0.007,d=3.22)

#This plot reproduces Figure 2b in Kelly et al. (2000)
plot(fine,type="l",xlim=c(0,1))
lines(coarse,lwd=2)
legend("topleft",c("coarse","fine"),lty=c(1,1),lwd=c(2,1),bty="n")
}
