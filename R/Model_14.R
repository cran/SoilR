#
# vim:set ff=unix expandtab ts=2 sw=2:
#setMethod(
#   f= "getAccumulatedRelease14",
#      ### This function integrates the release Flux over time
#      signature= "Model_14",
#      definition=function(object){
#      times=object@times
#      R=getReleaseFlux14(object)
#      n=ncol(R)
#      #transform the array to a list of functions of time by
#      #intepolating it with splines
#      if (n==1) {
#          Rfuns=list(splinefun(times,R))
#      }
#      else{
#        Rfuns=list(splinefun(times,R[,1]))
#        for (i in 2:n){
#            Rf=splinefun(times,R[,i])
#            Rfuns=append(Rfuns,Rf)
#        }
#      }
#      #test=Rfuns[[1]]
#      #now we can construct the derivative of the respiration as function of time
#      #as needed by the ode solver
#      rdot=function(y,t0){
#           # the simples possible case for an ode solver is that the ode is
#           # just an integral and does not depend on the value but only on t
#           # This is the case here
#           rv=matrix(nrow=n,ncol=1)
#           for (i in 1:n){
#               #print(Rfuns[i])
#               rv[i,1]=Rfuns[[i]](t0)
#           }
#           return(rv)
#      }
#      sVmat=matrix(0,nrow=n,ncol=1)
#      Y=solver(object@times,rdot,sVmat,object@solverfunc)
#      #### A matrix. Every column represents a pool and every row a point in time
#      return(Y)
#   }
#)

correctnessOfModel14=function#check for unreasonable input parameters to Model constructor
### The parameters used by the function \code{\link{GeneralModel_14}} in SoilR have a biological meaning, and therefore cannot be arbitrary.
### This functions tests some of the obvious constraints of the general model. 
### by calling \code{\link{correctnessOfModel}} and additionally takes care of the
### following 
(object ##<< the object to be tested
)
{
# first check the Model14 specific issues
supported_formats=c("Delta14C","AbsoluteFractionModern")
atm_c14=object@c14Fraction
if (class(atm_c14)!="FcAtm"){
   stop(simpleError("The object describing the atmospheric c_14 fraction must be of class FcAtm. This is a subclass of TimeMap additionally containing information about the format in which the values are given"))
}
f=atm_c14@format
if (!any(grepl(f,supported_formats))){
   err_str=paste("The required format:",f," describing the atmospheric c_14 fraction is not supported.\n 
	     The supported formats are: ",supported_formats,sep="")
   stop(simpleError(err_str))
}
t_min=min(object@times)
t_max=max(object@times)
tA_min=getTimeRange(atm_c14)["t_min"]
tA_max=getTimeRange(atm_c14)["t_max"]
    if (t_min<tA_min) {
        stop(simpleError("You ordered a timeinterval that starts earlier than the interval your atmospheric 14C fraction is defined for. \n Have look at the timeMap object or the data it is created from")
        )
    }
    if (t_max>tA_max) {
        stop(simpleError("You ordered a timeinterval that ends later than the interval your  your atmospheric 14C fraction is defined for. \n Have look at the timeMap object or the data it is created from")
        )
    }
# now check the things common to all Model objects 
res=correctnessOfModel(object)
}

##########################################################

    ### defines a representation of a 14C model
setClass(# Model_14
    Class="Model_14",
    contains="Model",
    representation=representation(
        #"Model",                          
        c14Fraction="TimeMap",
        c14DecayRate="numeric",
        initialValF="SoilR.F0"
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
    , validity=correctnessOfModel14 #set the validating function
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
        initialValF=new(Class="SoilR.F0",values=c(0),format="Delta14C")

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
        .Object@initialValF=initialValF
        .Object@inputFluxes=inputFluxes
        .Object@c14Fraction=c14Fraction
        .Object@c14DecayRate=c14DecayRate
        .Object@solverfunc=solverfunc
        if (pass==FALSE) validObject(.Object) #call of the ispector if not explicitly disabled
        return(.Object)
    }
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
      m=diag(rep(k,ns),nrow=ns) # Need to specify the dimension of the matrix otherwise doesn't work for n=1.
      A_C14=function(t){
          Aorg=A(t)
          newA=Aorg+m
          return(newA)
      }
      #get the Inputrate TimeMap 
      itm=object@inputFluxes
      input=getFunctionDefinition(itm)
      #get the C14 fraction Fctm (which is a subclass of TimeMap  
      Fctm=object@c14Fraction
      F0=object@initialValF
      # To do the computations we have to convert the atmospheric C14 fraction into 
      # a format that ensures that no negative values occur because this is assumed
      # by the algorithms that will blow up the solution if this assumption is not 
      # justified.
      # To this end we convert everything else to the "Absolute Fraction Modern" format
      # that ensures positive values
      Fctm=AbsoluteFractionModern(Fctm)
      F0=AbsoluteFractionModern(F0)
      
      Fc=getFunctionDefinition(Fctm)
      input_C14=function(t){
          #we compute the C14 fraction of the input
          return(Fc(t)*input(t))
      }
      ydot=NpYdot(A_C14,input_C14)
      #the initial Values have to be adopted also because
      #in the following computation they describe the intial amount of C_14
      #To do so we multiply them with the value of Fc at the begin of the computation 
      inivals=getValues(F0)
      inivalsFormat=getFormat(F0)
      sVmat=matrix(inivals*object@initialValues,nrow=ns,ncol=1) # We use here the Hadarmard (entry-wise) product instead of matrix multiplication
      Y=solver(object@times,ydot,sVmat,object@solverfunc) 
      ### A matrix. Every column represents a pool and every row a point in time
      return(Y)
   }
)
setMethod(
   f= "getF14",
      signature= "Model_14",
      definition=function(object){
      C=getC(object) ### we use the C14 here
      C14=getC14(object) ### we use the C14 here
      fr=C14/C
      # since the result is always in AbsoluteFractionModern wie have to convert it to Delta14C
      fr=Delta14C_from_AbsoluteFractionModern(fr)
      return(fr)
      ### A matrix. Every column represents a pool and every row a point in time
   }
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
setMethod(
  f= "getF14R",
  signature= "Model_14",
  definition=function(object){
    R=getReleaseFlux(object) ### we use the C14 here
    R14=getReleaseFlux14(object) ### we use the C14 here
    fr=rowSums(R14)/rowSums(R)
    # since the result is always in AbsoluteFractionModern wie have to convert it to Delta14C
    fr=Delta14C_from_AbsoluteFractionModern(fr)
    #print(dim(C))
    ### A matrix. Every column represents a pool and every row a point in time
    return(fr)
  }
  )
setMethod(
  f= "getF14C",
  signature= "Model_14",
  definition=function(object){
    C=getC(object) ### we use the C14 here
    C14=getC14(object) ### we use the C14 here
    fr=rowSums(C14)/rowSums(C)
    fr=Delta14C_from_AbsoluteFractionModern(fr)
    #print(dim(C))
    ### A matrix. Every column represents a pool and every row a point in time
    return(fr)
  }
  )

