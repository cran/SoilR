OnepModel14<-structure(
  function #Implementation of a one-pool C14 model
  ### This function creates a model for one pool. 
  ### It is a wrapper for the more general function \code{\link{GeneralModel_14}} that can handle an arbitrary number of pools.
  (t,    	##<< A vector containing the points in time where the solution is sought. It must be specified within the same period for which the Delta 14 C of the atmosphere is provided. The default period in the provided dataset \code{\link{C14Atm_NH}} is 1900-2010.
   k,	##<< A scalar with the decomposition rate of the pool. 
   C0,	##<<  A scalar containing the initial amount of carbon in the pool.
   In,     ##<< A scalar or a data.frame object specifying the amount of litter inputs by time.
   xi=1,   ##<< A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates. 
   FcAtm,##<< A Data Frame object consisting of  a function describing the fraction of C_14 in per mille.
   lambda=-0.0001209681, ##<< Radioactive decay constant. By default lambda=-0.0001209681 y^-1 . This has the side effect that all your time related data are treated as if the time unit was year.
   lag=0, ##<< A (positive) scalar representing a time lag for radiocarbon to enter the system. 
   solver=deSolve.lsoda.wrapper ##<< A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.
   )	
  { 
    t_start=min(t)
    t_stop=max(t)
    if(length(k)!=1) stop("k must be a scalar (length == 1)")
    if(length(C0)!=1) stop("initial conditions must be of length = 1")
    
    if(length(In)==1) inputFluxes=new("TimeMap",
                                      t_start,
                                      t_stop,
                                      function(t){matrix(nrow=1,ncol=1,In)}
                                      )
    if(class(In)=="data.frame"){
      x=In[,1]  
      y=In[,2]  
      inputFlux=function(t0){as.numeric(spline(x,y,xout=t0)[2])}
      inputFluxes=new("TimeMap",
                      t_start,
                      t_stop,
                      function(t){matrix(nrow=1,ncol=1,inputFlux(t),0)}
                      )   
    }
    
    if(length(xi)==1) fX=function(t){xi}
    if(class(xi)=="data.frame"){
      X=xi[,1]
      Y=xi[,2]
      fX=function(t){as.numeric(spline(X,Y,xout=t)[2])}
    }
    
    A=-abs(matrix(k,1,1))
    
    At=new(Class="DecompositionOperator",
           t_start,
           t_stop,
           function(t){
             fX(t)*A
           }
           ) 
    
    Fc=TimeMap.from.Dataframe(FcAtm,lag)
    
    mod=GeneralModel_14(t,At,ivList=C0,inputFluxes=inputFluxes,Fc,di=lambda)
    ### A Model Object that can be further queried 
    ##seealso<< \code{\link{OnepModel}}, \code{\link{TwopParallelModel14}}, \code{\link{TwopFeedbackModel14}} 
  }
  ,
  ex=function(){
    
    data(C14Atm_NH)
    #Fc=TimeMap.from.Dataframe(C14Atm_NH)
    years=seq(1901,2009,by=0.5)
    LitterInput=700 
    
    Ex=OnepModel14(t=years,k=1/10,C0=500, In=LitterInput, FcAtm=C14Atm_NH)
    C14t=getSoilC14Fraction(Ex)
    
    plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
    lines(years, C14t[,1], col=4)
    legend("topright",c("Delta 14C Atmosphere", "Delta 14C in SOM"),lty=c(1,1),col=c(1,4),lwd=c(1,1),bty="n")
    
  }
  )
