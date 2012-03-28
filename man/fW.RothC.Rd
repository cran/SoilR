\name{fW.RothC}
\alias{fW.RothC}
\title{Effects of moisture on decomposition rates according to the RothC model}
\description{Calculates the effects of moisture (precipitation and pan evaporation) on decomposition rates according to the RothC model.}
\usage{fW.RothC(P, E, S.Thick = 23, pClay = 23.4, pE = 0.75, bare = FALSE)}
\arguments{
  \item{P}{A vector with monthly precipitation (mm).}
  \item{E}{A vector with same length with open pan evaporation or evapotranspiration (mm).}
  \item{S.Thick}{Soil thickness in cm. Default for Rothamsted is 23 cm.}
  \item{pClay}{Percent clay.}
  \item{pE}{Evaporation coefficient. If open pan evaporation is used pE=0.75. If Potential evaporation is used, pE=1.0.}
  \item{bare}{Logical. Under bare soil conditions, bare=TRUE. Dafault is set under vegetated soil.}
}

\value{A data.frame with accumulated top soil moisture deficit (Acc.TSMD) and the rate modifying factor b. }
\references{Coleman, K., and D. S. Jenkinson (1999), RothC-26.3 A model for the turnover of carbon in soil: 
model description and windows user guide (modified 2008), 47 pp, IACR Rothamsted, Harpenden.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller <mamueller@bgc-jena.mpg.de>}




\examples{
P=c(74,59,62,51,52,57,34,55,58,56,75,71) #Monthly Precipitation (mm)
E=c(8,10,27,49,83,99,103,91,69,34,16,8)  #Monthly open pan evaporation (mm)

Rothamsted=fW.RothC(P,E)
data.frame(month.name,P,E,0.75*E,P-0.75*E,Rothamsted)  # This reproduces Table 1 in the RothC documentation (Coleman and Jenkinson 1999)

}
