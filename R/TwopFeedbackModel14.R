TwopFeedbackModel14<-structure(
  function #Implementation of a two-pool C14 model with feedback structure
  ### This function creates a model for two pools connected with feedback. 
  ### It is a wrapper for the more general function \code{\link{GeneralModel_14}} that can handle an arbitrary number of pools.
  (t,    	##<< A vector containing the points in time where the solution is sought. It must be specified within the same period for which the Delta 14 C of the atmosphere is provided. The default period in the provided dataset \code{\link{C14Atm_NH}} is 1900-2010.
   ks,	##<< A vector of length 2 containing the decomposition rates for the 2 pools. 
   C0,	##<< A vector of length 2 containing the initial amount of carbon for the 2 pools.
   In,     ##<< A scalar or a data.frame object specifying the amount of litter inputs by time.
   a21,  ##<< A scalar with the value of the transfer rate from pool 1 to pool 2.
   a12,  ##<< A scalar with the value of the transfer rate from pool 2 to pool 1.
   xi=1,   ##<< A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates. 
   FcAtm,##<< A Data Frame object consisting of  a function describing the fraction of C_14 in per mille.
   lambda=-0.0001209681, ##<< Radioactive decay constant. By default lambda=-0.0001209681 y^-1 . This has the side effect that all your time related data are treated as if the time unit was year.
   lag=0, ##<< A positive integer representing a time lag for radiocarbon to enter the system. 
   solver=deSolve.lsoda.wrapper, ##<< A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.
   pass=FALSE  ##<< Forces the constructor to create the model even if it is invalid 
   )	
  { 
    t_start=min(t)
    t_stop=max(t)
    if(length(ks)!=2) stop("ks must be of length = 2")
    if(length(C0)!=2) stop("the vector with initial conditions must be of length = 2")
    
    if(length(In)==1) inputFluxes=new("TimeMap",
                                      t_start,
                                      t_stop,
                                      function(t){matrix(nrow=2,ncol=1,c(In,0))}
                                      )
    if(class(In)=="data.frame"){
      x=In[,1]  
      y=In[,2]  
      inputFlux=function(t0){as.numeric(spline(x,y,xout=t0)[2])}
      inputFluxes=new("TimeMap",
                      t_start,
                      t_stop,
                      function(t){matrix(nrow=2,ncol=1,c(inputFlux(t),0))}
                      )   
    }
    
    if(length(xi)==1) fX=function(t){xi}
    if(class(xi)=="data.frame"){
      X=xi[,1]
      Y=xi[,2]
      fX=function(t){as.numeric(spline(X,Y,xout=t)[2])}
    }
    
    A=-abs(diag(ks))
    A[2,1]=a21
    A[1,2]=a12
    
    At=new(Class="DecompositionOperator",
           t_start,
           t_stop,
           function(t){
             fX(t)*A
           }
           ) 
    
    #Clumsy implementation of the time lag. This should be improved to allow scalar values instead of integers, most likely within the functions GeneralModel_14 or Model
    if(lag!=0) FcAtm=data.frame(FcAtm[-((length(FcAtm[,1])-(lag-1)):(length(FcAtm[,1]))),1],FcAtm[-(1:lag),2])
    
    Fc=TimeMap.from.Dataframe(FcAtm)
    
    mod=GeneralModel_14(t,At,ivList=C0,inputFluxes=inputFluxes,Fc,di=lambda)
    ### A Model Object that can be further queried 
    ##seealso<< \code{\link{TwopSeriesModel14}}, \code{\link{TwopParallelModel14}} 
  }
  ,
  ex=function(){
    
    data(C14Atm_NH)
    #Fc=TimeMap.from.Dataframe(C14Atm_NH)
    years=seq(1901,2009,by=0.5)
    LitterInput=700 
    
    Ex=TwopFeedbackModel14(t=years,ks=c(k1=1/2.8, k2=1/35),C0=c(200,5000), In=LitterInput, a21=0.1,a12=0.01,FcAtm=C14Atm_NH)
    R14m=getTotalReleaseFluxC14CRatio(Ex)
    C14m=getTotalC14CRatio(Ex)
    C14t=getSoilC14Fraction(Ex)
    
    par(mfrow=c(2,1))
    plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
    lines(years, C14t[,1], col=4)
    lines(years, C14t[,2],col=4,lwd=2)
    legend("topright",c("Delta 14C Atmosphere", "Delta 14C pool 1", "Delta 14C pool 2"),lty=c(1,1,1),col=c(1,4,4),lwd=c(1,1,2),bty="n")
    
    plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
    lines(years,C14m,col=4)
    lines(years,R14m,col=2)
    legend("topright",c("Delta 14C Atmosphere","Delta 14C SOM", "Delta 14C Respired"),lty=c(1,1,1), col=c(1,4,2),bty="n")
    par(mfrow=c(1,1))
  }
  )
