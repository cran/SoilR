TimeMap.new=function
### This function is the basic constructor of the class TimeMap.
(t_start, ##<<A number marking the begin of the time domain where the function is valid
 t_end,   ##<<A number the end of the time domain where the function is valid
 f        ##<<The time dependent function definition (a function in R's sense)
 ){
   obj=new(Class="TimeMap",t_start,t_end,f) 
return(obj)
### An object of class TimeMap that can be used to describe models.
}
deSolve.lsoda.wrapper=function(
### The function serves as a wrapper for lsoda using a much simpler interface which allows the use 
### of matrices in the definition of the derivative. 
### To use lsoda we have to convert our vectors to lists, define tolerances and so on.
### This function does this for us , so we don't need to bother about it.
	       t,	##<< A row vector containing the points in time where the solution is sought.
	       ydot,    ##<< The function of y and t that computes the derivative for a given 
	       ## point in time and a column vector y.
	       startValues ##<< A column vector with the starting values.
	       ){
   
   parms=NULL
   my.atol <- 1e-6
   rtol=1e-4
   lsexamp <- function(t, y,parms)
     {
	yv=cbind(y)
	YD=ydot(y,t)
	yd=as.vector(YD)
       #list(yd,c(massbalance=sum(y))) we could add other output parameter if we are interested
       list(yd)
     }
   require(deSolve)
   out <- lsoda(startValues,t,lsexamp, parms, rtol, atol= my.atol)
      #print(paste("out=",out))
      #print(out)
   # The output of lsoda is unsuiteable for our needs for two reasons
   # 1.) It also returns the time vector in column 1 
   # 2.) the columns get names instead of the default numbers created
   #     by the matrix function
   # we threrefore extract the information and store it in a new matrix witch will be t 
   n=length(startValues)
   if (n==1) { Yt=matrix(ncol=n,out[,-1])}
   else {Yt=out[,-1]}
   #print("Yt=")
   #print(Yt)
   #determine the number of pools 
   #determine the number of time values for which the solution is sought
   tn=length(t) 
   Y=matrix(ncol=n,nrow=length(t))
   #print(Yt[,1])
   for (i in 1:n){
      #print(paste("i=",i))
      Y[,i]=Yt[,i]
   }
   return(Y)
   ### A matrix. Every column represents a pool and every row a point in time
}
##########################################################################

### defines a (time dependent) mapping including the function definition and the ### domain where the function is well define.  This can be used to avoid interpolations out of range when mixing different time dependent data sets
setClass(
   Class="TimeMap",
   representation=representation(
	starttime="numeric"
    ,
	endtime="numeric"
    ,
    map="function"
   )
)
setMethod(
    f="initialize",
    signature="TimeMap",
    definition=function(.Object,starttime=numeric(),endtime=numeric(),map=function(t){t}){
    #cat("-initializer at work-\n")
    .Object@starttime=starttime
    .Object@endtime=endtime
    .Object@map=map
    return(.Object)
    }
)
setGeneric(
    name="getTimeRange",
    def=function(object){
    ### The function returns the time range of the given TimeMap object. 
        standardGeneric("getTimeRange")
    }
)
setMethod(
    f="getTimeRange",
    signature="TimeMap",
    definition=function(object){
        return(
               c("t_min"=object@starttime,"t_max"=object@endtime))
    }
)
setGeneric(
    name="getFunctionDefinition",
    def=function(object){
    ### extract the function definition (the R-function) from the argument
        standardGeneric("getFunctionDefinition")
    }
)
setMethod(
    f="getFunctionDefinition",
    signature="TimeMap",
    definition=function(object){
    ### extract the function definition (the R-function) from the TimeMap 
        return(object@map)
    }
)


TimeMap.from.Dataframe=function
### This function is another constructor of the class TimeMap.
(dframe, ##<<A data frame containing exactly two columns:
## the first one is interpreted as time
interpolation=splinefun ##<<A function that  returns a function  the default is splinefun. Other possible values are the linear interpolation approxfun or any self made function with the same interface.
 ){
   t=dframe[,1]  
   y=dframe[,2]  
   o=order(t)
   tyo=cbind(t[o],y[o])
   to=tyo[,1]
   yo=tyo[,2]
   t_start=min(to)
   interpol=splinefun(to,yo)
   t_start=min(t)
   t_end=max(t)
   interpol=interpolation(to,yo)
   obj=new(Class="TimeMap",t_start,t_end,interpol) 
return(obj)
### An object of class TimeMap that contains the interpolation function and the limits of the time range where the function is valid.
### this serves as a saveguard for Model which thus can check that all involved functions of time are actually defined for the times of interest  
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
)

setMethod(
    f="initialize",
    signature="Model",
    definition=function(
        .Object,times=numeric()
        ,
        mat=TimeMap.new(
                0,
                0,
                function(t){
                    return(matrix(nrow=1,ncol=1,1))
                }
        ) 
        ,
        initialValues=numeric()
        ,
        inputFluxes= TimeMap.new(
            0,
            0,
            function(t){
                return(matrix(nrow=1,ncol=1,1))
            }
        )
        ,
        solverfunc=deSolve.lsoda.wrapper
        ){
       # cat("-initializer at work-\n")
        .Object@times=times
        .Object@mat=mat
        .Object@initialValues=initialValues
        .Object@inputFluxes=inputFluxes
        .Object@solverfunc=solverfunc
        return(.Object)
    }
)
##defining Constructors
##The constructors are defined in seperate files which end with Model

setGeneric ( # This function 
   name= "getReleaseFlux",
   def=function(# access to the C content of the pools 
   ### This function computes the overall  carbon release of the given model as funtion of time 
	object
	){standardGeneric("getReleaseFlux")}
)
setGeneric ( # This function 
   name= "getRelease",
   def=function(# access to the C content of the pools 
   ### This function computes the overall  carbon release of the given model as funtion of time 
	object
	){standardGeneric("getRelease")}
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
setMethod(
   f= "getRelease",
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
##########################################################

    ### defines a representation of a 14C model
setClass(# Model_14
    Class="Model_14",
    representation=representation(
        c14Fraction="TimeMap",
        c14DecayRate="numeric"
    ),
    contains="Model"     
   )
setMethod(
    f="initialize",
    signature="Model_14",
    definition=function(
        .Object,times=numeric()
        ,
        mat=TimeMap.new(
                0,
                0,
                function(t){
                    return(matrix(nrow=1,ncol=1,1))
                }
        ) 
        ,
        initialValues=numeric()
        ,
        inputFluxes= TimeMap.new(
            0,
            0,
            function(t){
                return(matrix(nrow=1,ncol=1,1))
            }
        )
        ,
        c14Fraction=TimeMap()
        ,
        c14DecayRate=0
        ,
        solverfunc=deSolve.lsoda.wrapper
     ){
       # cat("-initializer at work-\n")
        .Object@times=times
        .Object@mat=mat
        .Object@initialValues=initialValues
        .Object@inputFluxes=inputFluxes
        .Object@c14Fraction=c14Fraction
        .Object@c14DecayRate=c14DecayRate
        .Object@solverfunc=solverfunc
        return(.Object)
    }
)
setGeneric ( # This function 
   name= "getRelease14",
   def=function(# access to the C content of the pools 
   ### This function computes the overall  14C  release of the given model as funtion of time 
	object
	){standardGeneric("getRelease14")}
)
setMethod(
   f= "getRelease14",
      ### This function integrates the release Flux over time
      signature= "Model_14",
      definition=function(object){
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
setGeneric ( # This function 
   name= "getC14",
   def=function(# access to the C content of the pools 
    ### This function computes the value for C (mass or concentration ) as function of time
	object
	){standardGeneric("getC14")}
)
setMethod(
   f= "getC14",
      signature= "Model_14",
      definition=function(object){
      ### This function computes the value for C (mass or concentration ) as function of time
      ns=length(object@initialValues)
      #get the coefficient matrix TimeMap 
      Atm=object@mat
      A=getFunctionDefinition(Atm)
      # add the C14 decay to the matrix which is done by a diagonal matrix which does not vary over time
      # we assume a half life of th=5730 years
      th=5730
      k=log(0.5)/th #note that k is negative
      m=matrix(ncol=ns,nrow=ns,0)
      for (i in 1:ns){m[[i,i]]=k}
      A_C14=function(t){
          Aorg=A(t)
          newA=Aorg+m
          return(newA)
      }
      #get the Inputrate TimeMap 
      itm=object@inputFluxes
      input=getFunctionDefinition(itm)
      #get the C14 fraction matrix TimeMap 
      Fctm=object@mat
      Fc=getFunctionDefinition(Fctm)
      input_C14=function(t){
          #we compute the C14 fraction of the input
          return(Fc(t)*input(t))
      }
      ydot=NpYdot(A_C14,input_C14)
      #the initial Values have to be adopted also because
      #in the following computation they describe the intial amount of C_14
      #To do so we
      sVmat=matrix(object@initialValues,nrow=ns,ncol=1)
      Y=solver(object@times,ydot,sVmat,object@solverfunc) 
      #print(Y)
      ### A matrix. Every column represents a pool and every row a point in time
      return(Y)
   }
)
