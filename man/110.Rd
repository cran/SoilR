\name{bind.C14curves}
\alias{bind.C14curves}
\title{Binding of pre- and post-bomb Delta14C curves}
\description{This function takes a pre- and a post-bomb curve, binds them together, and reports the results back either in years BP or AD.}
\usage{bind.C14curves(prebomb, postbomb, time.scale)}\arguments{
  \item{prebomb}{A pre-bomb radiocarbon dataset. They could be either \code{\link{IntCal09}} or \code{\link{IntCal13}}.}
  \item{postbomb}{A post-bomb radiocarbon dataset. They could be any of the datasets in \code{\link{Hua2013}}.}
  \item{time.scale}{A character indicating whether to report the results in years before present \code{BP} or anno domini \code{AD}.}
}

\value{A \code{data.frame} with 3 columns: years in AD or BP, the atmospheric Delta14C value, the standard deviation of the Delta14C value.}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{
#Bind the IntCal13 dataset and Hua2013 for the NH Zone 1
bp=bind.C14curves(prebomb=IntCal13,postbomb=Hua2013$NHZone1,time.scale="BP")

plot(bp[,1:2],type="l")
plot(bp[,1:2],type="l",xlim=c(-100,100))

#Report results in years AD
ad=bind.C14curves(prebomb=IntCal13,postbomb=Hua2013$NHZone1,time.scale="AD")

plot(ad[,1:2],type="l")
plot(ad[,1:2],type="l",xlim=c(0,2010))
abline(v=1950,lty=2)
}
