\name{CourseExample_R14}
\alias{CourseExample_R14}
\docType{data}
\title{
An example data set for parameter estimation 
}
\description{
    The dataset contains two dataframes describing the $^{14}C$ fraction of the Respiration and the C content of two different pools.  
}
\usage{data(CourseExample_R14)}
\format{
  two data frames with 
  \describe{
    \item{\code{DataC}}{a data frame}
    \item{\code{DataR14}}{a data frame}
  }
}
\examples{
\dontrun{
#requires package ggplot2
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

p <- p+opts(legend.title=theme_blank())
################
yl=max(DataR14[,"R14t"])
ym=min(DataR14[,"R14t"])
limits<- aes(ymax = R14t + sd, ymin=R14t - sd)
p <- ggplot(data.frame(DataR14), aes(colour=c(1), y=R14t, x=time))
p + geom_point() + geom_errorbar(limits, width=0.2)
p + geom_pointrange(limits)
}
}
\keyword{datasets}
