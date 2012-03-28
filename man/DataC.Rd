\name{DataC}
\alias{DataC}
\docType{data}
\title{
An example data set for parameter estimation 
}
\description{
    The dataset contains dataframe describing the  C stocks of two different pools.  
}
\usage{data(CourseExample_R14)}
\format{
  A data frame with 4 variables
  \describe{
    \item{\code{C1}}{ C stock pool 1}
    \item{\code{C2}}{ C stock pool 2}
    \item{\code{sd}}{the error}
    \item{\code{time}}{the year of the measurement}
  }
}
\examples{
library(ggplot2)
data(CourseExample_R14)
lty1=1
lty2=2
c1="pool 1"
c2="pool 2"
yl=max(DataC[,"C1"],DataC[,"C2"])
ym=min(DataC[,"C1"],DataC[,"C2"])

p <- ggplot(data.frame(DataC))
p <- p+geom_point(aes(x=time,y=C1,col=c1)) 
p <- p+geom_errorbar(aes(x=time,ymin=C1-sd,ymax=C1+sd,col=c1))
p <- p+geom_point(aes(x=time,y=C2,col=c2)) 
p <- p+geom_errorbar(aes(x=time,ymin=C2-sd,ymax=C2+sd,col=c2))
p <- p+scale_y_continuous(name="C content of the pools")

}
\keyword{datasets}
