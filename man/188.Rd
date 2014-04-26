\name{getTimeRange_method__TimeMap}
\alias{getTimeRange_method__TimeMap}
\title{ask for the boundaries of the underlying time interval}
\description{The method returns the time range of the given object 
It is probably mostly used internally to make sure that 
time dependent functions retrieved from data are not
used outside the interval where they are valid. }
\arguments{
  \item{object}{An object of class TimeMap or one that inherits from TimeMap}
}

\value{a vector of length two \code{ c(t_min,t_max) }
containing start and end time of the time interval 
for which the TimeMap object has been defined.}

\author{Carlos A. Sierra <csierra@bgc-jena.mpg.de>, Markus Mueller
<mamueller@bgc-jena.mpg.de>}




\examples{# set up some variables 
t_start=0 
t_end=10 

# Create an explicit example of a TimeMap
c0=c(0.5, 0.5, 0.5)
inputFluxes=TimeMap.new(
  t_start,
  t_end,
  function(t0){matrix(nrow=n,ncol=1,c(0.0,0,0))}
) 
# now it you only have the inputFluxes
# you can ask it for which time interval it was specified

print(getTimeRange(inputFluxes))


# Construct a less explicit example of a TimeMap  with an object 
# of class # BoundLinDecompOp which is subclass of TimeMap
n=3
At=new("BoundLinDecompOp",
  t_start,
  t_end,
  function(t0){
        matrix(nrow=n,ncol=n,byrow=TRUE,
          c(-0.2,    0,    0, 
             0  , -0.3,    0,   
             0,      0,   -0.4/t0)
        )
  }
) 
print(getTimeRange(At))
}
