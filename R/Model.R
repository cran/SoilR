correctnessOfModel=function
### The parameters used by the function \code{\link{GeneralModel}} in SoilR have a biological meaning, and therefore cannot be arbitrary.
### This functions tests some of the obvious constraints of the general model. 
### Up to now these are:
### 1) The compatibility of the decomposition rates and the transport parameters to and from other pools, i.e. 
### the column-wise sum of the elements cannot be negative. Otherwise this would create negative values of respiration, which are not biologically meaningful.
### 2) The compatibility of the time ranges of the supplied functions 

(object)
{   
    times=object@times
    Atm=object@mat
    ivList=object@initialValues
    InputFluxes=object@inputFluxes
    #first we check the dimensions
    A=getFunctionDefinition(Atm)
    na=nrow(A(0))
    #compute the respiration coefficients as funtions of time
    rcoeffs=RespirationCoefficients(A)
    r=sapply(times,rcoeffs)
    #mark the negative respirations (which will trigger the refusal of the matrix )
    truthv=sapply(r,is.negative)
    #find the bad columns 
    positions=grep("TRUE",truthv)
    res=TRUE
    if (length(positions)>0){
       stop(simpleError("The following columns contain unreasonable entries that lead to negative respirations for these pools. Please check your matrix as function of time."))
        }
     
    tA_min=getTimeRange(Atm)["t_min"]
    tA_max=getTimeRange(Atm)["t_max"]
    tI_min=getTimeRange(InputFluxes)["t_min"]
    tI_max=getTimeRange(InputFluxes)["t_max"]
    t_min=min(times)
    t_max=max(times)
    if (t_min<tA_min) {
        stop(simpleError("You ordered a timeinterval that starts earlier than the interval your matrix valued function A(t) is defined for. \n Have look at the timeMap object of A(t) or the data it is created from")
        )
    }
    if (t_max>tA_max) {
        stop(simpleError("You ordered a timeinterval that ends later than the interval your matrix valued function A(t) is defined for. \n Have look at the timeMap object of A(t) or the data it is created from")
        )
    }
    if (t_min<tI_min) {
        stop(simpleError("You ordered a timeinterval that starts earlier than the interval your function I(t) (InputFluxes) is defined for. \n Have look at the timeMap object of I(t) or the data it is created from")
        )
    }
    if (t_max>tI_max) {
        stop(simpleError("You ordered a timeinterval that ends later than the interval your function I(t) (InputFluxes) is defined for. \n Have look at the timeMap object of I(t) or the data it is created from")
        )
    }

    return(res)
}
is.negative=function(number){
   ### the function returns True if the argumente is negative
   return(number<0)
}
### serves as a fence to the interface of SoilR functions. So that later implementations can differ	 
setClass(# Model
   Class="Model",
   representation=representation(
        times="numeric"
        ,
        mat="TimeMap"
        ,
        initialValues="numeric"
        ,
        inputFluxes="TimeMap"
        ,
        solverfunc="function"
   )
#   ,
#   prototype=prototype(
#        times=c(0,1),
#        mat=TimeMap.new(
#            0,
#            1,
#            function(t){
#                return(matrix(nrow=1,ncol=1,1))
#            }
#        ) 
#        ,
#        initialValues=numeric()
#        ,
#        inputFluxes= TimeMap.new(
#            0,
#            1,
#            function(t){
#                return(matrix(nrow=1,ncol=1,1))
#            }
#        )
#        ,
#        solverfunc=deSolve.lsoda.wrapper
#    )
#
    , validity=correctnessOfModel #set the validating function
)


setMethod(
    f="initialize",
    signature="Model",
    definition=function(
        .Object,
        times=c(0,1),
        mat=TimeMap.new(
                0,
                1,
                function(t){
                    return(matrix(nrow=1,ncol=1,0))
                }
        ) 
        ,
        initialValues=numeric()
        ,
        inputFluxes= TimeMap.new(
            0,
            1,
            function(t){
                return(matrix(nrow=1,ncol=1,1))
            }
        )
        ,
        solverfunc=deSolve.lsoda.wrapper
        ,
        pass=FALSE
        ){
       # cat("-initializer at work-\n")
        .Object@times=times
        .Object@mat=mat
        .Object@initialValues=initialValues
        .Object@inputFluxes=inputFluxes
        .Object@solverfunc=solverfunc
        #if (pass==FALSE) validObject(.Object) #call of the ispector if not explicitly disabled
        if (pass==FALSE) correctnessOfModel(.Object) #call of the ispector if not explicitly disabled
        return(.Object)
    }
)
##defining Constructors
##The constructors are defined in seperate files which end with Model

setGeneric ( # This function 
   name= "getTimes",
   def=function(# access to the time values the model solution is sougth for 
    ### This functions extracts the times argument
	object
	){standardGeneric("getTimes")}
)
setMethod(
   f= "getTimes",
      signature= "Model",
      definition=function(object){
      ### This functions extracts the times argument from an argument of class Model
      return(object@times)
   }
)
setGeneric ( # This function 
   name= "getC",
   def=function(# access to the C content of the pools 
    ### This function computes the value for C (mass or concentration ) as function of time
	object
	){standardGeneric("getC")}
)
setMethod(
   f= "getC",
      signature= "Model",
      definition=function(object){
      ### This function computes the value for C (mass or concentration ) as function of time
      ns=length(object@initialValues)
      Atm=object@mat
      #print(Atm)
      A=getFunctionDefinition(Atm)
      #print(A)
      itm=object@inputFluxes
      input=getFunctionDefinition(itm)
      #print(input)
      ydot=NpYdot(A,input)
      #print(ydot)
      sVmat=matrix(object@initialValues,nrow=ns,ncol=1)
      Y=solver(object@times,ydot,sVmat,object@solverfunc) 
      #print(Y)
      ### A matrix. Every column represents a pool and every row a point in time
      return(Y)
   }
)
setGeneric ( # This function 
   name= "getReleaseFlux",
   def=function(# access to the C content of the pools 
   ### This function computes the overall  carbon release of the given model as funtion of time 
	object
	){standardGeneric("getReleaseFlux")}
)
setMethod(
   f= "getReleaseFlux",
      signature= "Model",
      definition=function(object){
      C=getC(object)
      #print("dim(C)=")
      #print(dim(C))
      times=object@times
      Atm=object@mat
      A=getFunctionDefinition(Atm)
      n=length(object@initialValues)
      #print(n)
      rfunc=RespirationCoefficients(A)
      #rfunc is vector valued function of time
      if (n==1) { r=matrix(ncol=n,sapply(times,rfunc))}
      else {r=t(sapply(times,rfunc))}
      #print("dim(r)=")
      #print(dim(r))
      R=r*C
      ### A matrix. Every column represents a pool and every row a point in time
      return(R)
   }
)
setGeneric ( # This function 
   name= "getAccumulatedRelease",
   def=function(# access to the C content of the pools 
   ### This function computes the overall  carbon release of the given model as funtion of time 
	object
	){standardGeneric("getAccumulatedRelease")}
)
setMethod(
   f= "getAccumulatedRelease",
      signature= "Model",
      definition=function(object){
      ### This function integrates the release Flux over time
      times=object@times
      R=getReleaseFlux(object)
      n=ncol(R)
      #transform the array to a list of functions of time by
      #intepolating it with splines
      if (n==1) {
          Rfuns=list(splinefun(times,R))
      }
      else{
        Rfuns=list(splinefun(times,R[,1]))
        for (i in 2:n){
            Rf=splinefun(times,R[,i])
            Rfuns=append(Rfuns,Rf)
        }
      }
      #test=Rfuns[[1]]
      #now we can construct the derivative of the respiration as function of time
      #as needed by the ode solver
      rdot=function(y,t0){
           # the simples possible case for an ode solver is that the ode is
           # just an integral and does not depend on the value but only on t
           # This is the case here
           rv=matrix(nrow=n,ncol=1)
           for (i in 1:n){
               #print(Rfuns[i])
               rv[i,1]=Rfuns[[i]](t0)
           }
           return(rv)
      }
      sVmat=matrix(0,nrow=n,ncol=1)
      Y=solver(object@times,rdot,sVmat,object@solverfunc)
      #### A matrix. Every column represents a pool and every row a point in time
      return(Y)
   }
)
##########################################################
