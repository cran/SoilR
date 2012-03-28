##########################################################

    ### defines a representation of a 14C model
setClass(# Model_14
    Class="Model_14",
    contains="Model",
    representation=representation(
        #"Model",                          
        c14Fraction="TimeMap",
        c14DecayRate="numeric"
    )
   #,
   #prototype=prototype(
   #     times=c(0,1),
   #     mat=TimeMap.new(
   #         0,
   #         1,
   #         function(t){
   #             return(matrix(nrow=1,ncol=1,1))
   #         }
   #     ) 
   #     ,
   #     initialValues=numeric()
   #     ,
   #     inputFluxes= TimeMap.new(
   #         0,
   #         1,
   #         function(t){
   #             return(matrix(nrow=1,ncol=1,1))
   #         }
   #     )
   #     ,
   #     c14Fraction=TimeMap.new(
   #         0,
   #         1,
   #         function(t){
   #             return(matrix(nrow=1,ncol=1,1))
   #         }
   #     )
   #     ,
   #     c14DecayRate=0
   #     ,
   #     solverfunc=deSolve.lsoda.wrapper
   #  )
    , validity=correctnessOfModel #set the validating function
)
setMethod(
    f="initialize",
    signature="Model_14",
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
        c14Fraction=TimeMap.new(
            0,
            1,
            function(t){
                return(matrix(nrow=1,ncol=1,1))
            }
        )
        ,
        c14DecayRate=0
        ,
        solverfunc=deSolve.lsoda.wrapper
        ,
        pass=FALSE
     ){
        .Object@times=times
        .Object@mat=mat
        .Object@initialValues=initialValues
        .Object@inputFluxes=inputFluxes
        .Object@c14Fraction=c14Fraction
        .Object@c14DecayRate=c14DecayRate
        .Object@solverfunc=solverfunc
        if (pass==FALSE) validObject(.Object) #call of the ispector if not explicitly disabled
        return(.Object)
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
      k=object@c14DecayRate
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
      Fctm=object@c14Fraction
      Fc=getFunctionDefinition(Fctm)
      input_C14=function(t){
          #we compute the C14 fraction of the input
          return(Fc(t)*input(t))
      }
      ydot=NpYdot(A_C14,input_C14)
      #the initial Values have to be adopted also because
      #in the following computation they describe the intial amount of C_14
      #To do so we multiply them with the value of Fc at the begin of the computation 
      sVmat=Fc(min(object@times))*matrix(object@initialValues,nrow=ns,ncol=1)
      Y=solver(object@times,ydot,sVmat,object@solverfunc) 
      #print(Y)
      ### A matrix. Every column represents a pool and every row a point in time
      return(Y)
   }
)
setGeneric ( # This function 
   name= "getSoilC14Fraction",
   def=function(# access to the C content of the pools 
   ### This function computes the \eqn{\frac{^{14}C}{C}}{14C/C} ratio in the soil as funtion of time 
	object
	){standardGeneric("getSoilC14Fraction")}
)
setMethod(
   f= "getSoilC14Fraction",
      signature= "Model_14",
      definition=function(object){
      C=getC(object) ### we use the C14 here
      C14=getC14(object) ### we use the C14 here
      fr=C14/C
      #print(dim(C))
      ### A matrix. Every column represents a pool and every row a point in time
      return(fr)
   }
)

setGeneric ( # This function 
   name= "getAccumulatedRelease14",
   def=function(# access to the C content of the pools 
   ### This function computes the overall  14C  release of the given model as funtion of time 
	object
	){standardGeneric("getAccumulatedRelease14")}
)
setGeneric ( # This function 
   name= "getReleaseFlux14",
   def=function(# access to the C content of the pools 
   ### This function computes the \eqn{^{14}C}{14C} release of the given model as funtion of time 
	object
	){standardGeneric("getReleaseFlux14")}
)
setMethod(
   f= "getReleaseFlux14",
      signature= "Model_14",
      definition=function(object){
      C=getC14(object) ### we use the C14 here
      #print("dim(C)=")
      #print(dim(C))
      times=object@times
      Atm=object@mat
      A=getFunctionDefinition(Atm)
      n=length(object@initialValues)
      #print(n)
      ### note that the respiration coefficients for 14C do not change in comparison 
      ### to the total C case
      ### The fraction of 14C lost by respiration is not greater for 14C 
      ### although the decay is faster due to the contribution of radioactivity
      rfunc=RespirationCoefficients(A)
      #rfunc is vector valued function of time
      if (n==1) { r=matrix(ncol=n,sapply(times,rfunc))}
      else {r=t(sapply(times,rfunc))}
      #print("dim(r)=")
      #print(dim(r))
      R=r*C
      # now compute the sum of every row
      

      ### A matrix. Every column represents a pool and every row a point in time
      return(R)
   }
)


#Added by C. Sierra, 28/4/2012
setGeneric ( # This function 
  name= "getTotalReleaseFluxC14CRatio",
  def=function(# access to the C release flux from the pools 
    ### This function computes the \eqn{\frac{^{14}C}{C}}{14C/C} ratio of the released C as funtion of time 
    object
    ){standardGeneric("getTotalReleaseFluxC14CRatio")}
  )
setMethod(
  f= "getTotalReleaseFluxC14CRatio",
  signature= "Model_14",
  definition=function(object){
    R=getReleaseFlux(object) ### we use the C14 here
    R14=getReleaseFlux14(object) ### we use the C14 here
    fr=rowSums(R14)/rowSums(R)
    #print(dim(C))
    ### A matrix. Every column represents a pool and every row a point in time
    return(fr)
  }
  )
setGeneric ( # This function 
  name= "getTotalC14CRatio",
  def=function(# access to the C release flux from the pools 
    ### This function computes the \eqn{\frac{^{14}C}{C}}{14C/C} ratio of the released C as funtion of time 
    object
    ){standardGeneric("getTotalC14CRatio")}
  )
setMethod(
  f= "getTotalC14CRatio",
  signature= "Model_14",
  definition=function(object){
    C=getC(object) ### we use the C14 here
    C14=getC14(object) ### we use the C14 here
    fr=rowSums(C14)/rowSums(C)
    #print(dim(C))
    ### A matrix. Every column represents a pool and every row a point in time
    return(fr)
  }
  )


setGeneric ( # This function 
   name= "getAccumulatedRelease14",
   def=function(# access to the C content of the pools 
   ### This function computes the overall  14C  release of the given model as funtion of time 
	object
	){standardGeneric("getAccumulatedRelease14")}
)
setMethod(
   f= "getAccumulatedRelease14",
      ### This function integrates the release Flux over time
      signature= "Model_14",
      definition=function(object){
      times=object@times
      R=getReleaseFlux14(object)
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
