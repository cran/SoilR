\name{fW.Daycent2}
\alias{fW.Daycent2}
\title{Effects of moisture on decomposition rates according to the DAYCENT model}
\description{Calculates the effects of volumetric water content on decomposition rates according to the Daycent/Century models.}
\usage{fW.Daycent2(W, WP = 0, FC = 100)}\arguments{
  \item{W}{A scalar or vector of volumetric water content in percentage.}
  \item{WP}{A scalar representing the wilting point in percentage.}
  \item{FC}{A scalar representing the field capacity in percentage.}
}

\value{A data frame with values of relative water content (RWC) and the effects of RWC on decomposition rates (fRWC).}
\references{Del Grosso, S. J., W. J. Parton, A. R. Mosier, E. A. Holland, E. Pendall, D. S. Schimel, and D. S. Ojima (2005), 
Modeling soil CO2 emissions from ecosystems, Biogeochemistry, 73(1), 71-91.}
\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{
W=10:90
fW=fW.Daycent2(W,WP=10,FC=90)
plot(fW,type="l",ylim=c(0,6)) #This plot reproduces Figure 1b, in del Grosso et al. (2005)
}
