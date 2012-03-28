GeneralModel=structure(function #The most general costructor for class Model
### The function creates a numerical model  for n arbitrarily connected pools.
### It is one of the constructors of class Model                       
### It is e.g. used by some more specialized wrapper functions like for instance \code{\link{ParallelModel}} but can also be used directly. It is in fact the most### versatile interface to produce instances of class Model. 

(t,			##<< A vector containing the points in time where the solution is sought.
 A,			##<< A TimeMap object consisting of  a matrix valued function describing the whole model decay rates for the n pools, connection and feedback coefficients as functions of time and a time range for which this function is valid. The size of the quadtratic matric must be equal to the number of pools. The time range must cover the times given in the first argument. 
 ivList,		##<< A vector containing the initial amount of carbon for the n pools. The length of this vector is equal to the number of pools and thus equal to the length of k. This is checked by the function \code{\link{correctnessOfModel}}.
 inputFluxes, ##<< A TimeMap object consisting of a vector valued function describing the inputs to the pools as funtions of time \code{\link{TimeMap.new}}.
 solverfunc=deSolve.lsoda.wrapper,		##<< The function used by to actually solve the ODE system. This can be \code{\link{SoilR.euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface. 
 pass=FALSE  ##<< Forces the constructor to create the model even if it is invalid 
 )
{
   # first we test that the input values are compatible with each other
   obj=new(Class="Model",t,A,ivList,inputFluxes,solverfunc,pass)
   return(obj)
   ### A model object that can be further queried. 
   ##seealso<< \code{\link{TwopParallelModel}} 
}
,ex=function(){
      t_start=0 
      t_end=10 
      tn=50
      timestep=(t_end-t_start)/tn 
      t=seq(t_start,t_end,timestep) 
      n=3
      At=new("TimeMap",
        t_start,
        t_end,
        function(t0){
              #matrix(nrow=n,ncol=n,byrow=TRUE,
              #  c(-0.2,    0,    0, 
              #     0.1, -0.7,    0,   
              #     0,    1/2,   -0.5)
              #)
              matrix(nrow=n,ncol=n,byrow=TRUE,
                c(-0.2,    0,    0, 
                   0  , -0.3,    0,   
                   0,      0,   -0.4)
              )
        }
      ) 
      
      c0=c(0.5, 0.5, 0.5)
      #constant inputrate
      inputFluxes=TimeMap.new(
        t_start,
        t_end,
        function(t0){matrix(nrow=n,ncol=1,c(0.0,0,0))}
      ) 
      mod=GeneralModel(t,At,c0,inputFluxes)
      Y=getC(mod)
      lt1=1;  lt2=2; lt3=3 
      col1=1;  col2=2; col3=3
      plot(t,Y[,1],type="l",lty=lt1,col=col1,
           ylab="C stocks (arbitrary units)",xlab="Time") 
      lines(t,Y[,2],type="l",lty=lt2,col=col2) 
      lines(t,Y[,3],type="l",lty=lt3,col=col3) 
      legend(
         "topright",
         c("C in pool 1",
           "C in pool 2",
           "C in pool 3"
         ),
         lty=c(lt1,lt2,lt3),
         col=c(col1,col2,col3)
      )
#now compute the accumulated release
      Y=getAccumulatedRelease(mod)
      plot(t,Y[,1],type="l",lty=lt1,col=col1,ylab="C Release (arbitrary units)",xlab="Time") 
      lines(t,Y[,2],lt2,type="l",lty=lt2,col=col2) 
      lines(t,Y[,3],type="l",lty=lt3,col=col3) 
      legend("topleft",c("R1","R2","R3"),lty=c(lt1,lt2,lt3),col=c(col1,col2,col3))
 
}       
)
