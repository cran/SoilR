\name{getC}
\alias{getC}
\title{Calculates the C content of the pools }
\description{This function computes the carbon content of the pools as function of time.
Have a look at the methods for details.}
\usage{getC(object, as.closures = F)}\arguments{
  \item{object}{some model object, the actual class depends on the method used.}
  \item{as.closures}{if set to TRUE instead of a matrix a list of functions will be returned.}
}
\details{This function takes a Model object, which represents a system of ODEs 
and solves the system for \eqn{\mathbf{C}(t)}{C(t)}. The numerical solver used can be specified in the constructors of the Model classes
e.g. \code{\link{Model}},\code{\link{Model_14}},\code{\link{GeneralModel}}.}
\value{A matrix with m columns representing the number of pools, and n rows representing the times as specified by the argument
\code{t} in \code{\link{GeneralModel}} or another model creating function.}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}



\seealso{See examples in \code{\link{GeneralModel}}, \code{\link{GeneralModel_14}}, \code{\link{TwopParallelModel}}, 
\code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}}, etc.}
\examples{# This test function produces the example for testing the function getC
  t=seq(0,10,by=0.1) 
  k=0.8
  C0=100
  In = 30
  
  #Create two models with same arguments but calling two different model creation functions
  Cmodel=OnepModel(t,k,C0,In)
  C14model=OnepModel14(t,k,C0,In,F0=0,inputFc=IntCal09)
  
  #getC can extract the amount of C from these two type of models
  Ctmodel=getC(Cmodel)
  Ctmodel14=getC(C14model)
  
  #The output is identical because parameter values are the same
  plot(t,Ctmodel,type="l")
  lines(t,Ctmodel14,col=2,lty=2,lwd=2)
  legend("topright",c("OnepModel output","OnepModel14 output"),col=1:2,lty=1:2,bty="n")}
