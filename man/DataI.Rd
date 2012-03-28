\name{DataI}
\alias{DataI}
\docType{data}
\title{
An example data set for parameter estimation 
}
\description{
    The dataset contains dataframe describing the overall input to a two pool model.
}
\usage{data(CourseExample_R14)}
\format{
  A data frame with 2 variables
  \describe{
    \item{\code{time}}{the year of the measurement}
    \item{\code{In}}{sum of inputrates of both pools}
  }
}
\examples{
library(ggplot2)
data(CourseExample_R14)
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
################
p <- ggplot(DataI)
p <- p+geom_point(aes(x=time,y=In,col="inputrate ")) 
p <- p+scale_y_continuous(name="sum of input/y")
p <- p+opts(legend.title=theme_blank())
p

}
\keyword{datasets}
