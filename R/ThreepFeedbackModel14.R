#
# vim:set ff=unix expandtab ts=2 sw=2:
ThreepFeedbackModel14<-structure(
  function #Implementation of a three-pool C14 model with feedback structure
  ### This function creates a model for three pools connected with feedback. 
  ### It is a wrapper for the more general function \code{\link{GeneralModel_14}} that can handle an arbitrary number of pools with arbitrary connection. \code{\link{GeneralModel_14}} can also handle input data in different formats, while this function requires its input as Delta14C. Look at it as an example how to use the more powerful tool \code{\link{GeneralModel_14}} or as a shortcut for a standard task! 
  (t,      ##<< A vector containing the points in time where the solution is sought. It must be specified within the same period for which the Delta 14 C of the atmosphere is provided. The default period in the provided dataset \code{\link{C14Atm_NH}} is 1900-2010.
   ks,	##<< A vector of length 3 containing the decomposition rates for the 3 pools. 
   C0,	##<< A vector of length 3 containing the initial amount of carbon for the 3 pools.
   F0_Delta14C,  ##<< A vector of length 3 containing the initial fraction of radiocarbon for the 3 pools in Delta14C format.
        ####The format will be assumed to be Delta14C, so please take care that it is.
   In,     ##<< A scalar or a data.frame object specifying the amount of litter inputs by time.
   a21,  ##<< A scalar with the value of the transfer rate from pool 1 to pool 2.
   a12,  ##<< A scalar with the value of the transfer rate from pool 2 to pool 1.
   a32,  ##<< A scalar with the value of the transfer rate from pool 2 to pool 3.
   a23,  ##<< A scalar with the value of the transfer rate from pool 3 to pool 2.
   xi=1,   ##<< A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates. 
   FcAtm,##<< A Data Frame object containing values of atmospheric Delta14C per time. First column must be time values, second column must be Delta14C values in per mil.
   lambda=-0.0001209681, ##<< Radioactive decay constant. By default lambda=-0.0001209681 y^-1 . This has the side effect that all your time related data are treated as if the time unit was year.
   lag=0, ##<< A positive scalar representing a time lag for radiocarbon to enter the system. 
   solver=deSolve.lsoda.wrapper, ##<< A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.
   pass=FALSE  ##<< if TRUE forces the constructor to create the model even if it is invalid. This is sometimes useful when SoilR is used by externel packages for parameter estimation.  
   )	
{ 
    t_start=min(t)
    t_stop=max(t)
    if(length(ks)!=3) stop("ks must be of length = 2")
    if(length(C0)!=3) stop("the vector with initial conditions must be of length = 2")
    
    if(length(In)==1) inputFluxes=new("TimeMap",
                                      t_start,
                                      t_stop,
                                      function(t){matrix(nrow=3,ncol=1,c(In,0,0))}
                                      )
    if(class(In)=="data.frame"){
      x=In[,1]  
      y=In[,2]  
      inputFlux=function(t0){as.numeric(spline(x,y,xout=t0)[2])}
      inputFluxes=new("TimeMap",
                      t_start,
                      t_stop,
                      function(t){matrix(nrow=3,ncol=1,c(inputFlux(t),0,0))}
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
    A[3,2]=a32
    A[2,3]=a23
    
    At=new(Class="DecompositionOperator",
           t_start,
           t_stop,
           function(t){
             fX(t)*A
           }
           ) 
    
    Fc=FcAtm.from.Dataframe(FcAtm,lag,format="Delta14C")
    
    mod=GeneralModel_14(t,At,ivList=C0,initialValF=SoilR.F0.new(F0_Delta14C,"Delta14C"),inputFluxes=inputFluxes,Fc,di=lambda,solver,pass)
    ### A Model Object that can be further queried 
    ##seealso<<  \code{\link{GeneralModel_14}} \code{\link{ThreepSeriesModel14}}, \code{\link{ThreepParallelModel14}} 
  }
  ,
  ex=function(){
    
    years=seq(1901,2009,by=0.5)
    LitterInput=100
    k1=1/2; k2=1/10; k3=1/50
    
    Feedback=ThreepFeedbackModel14(
      t=years,
      ks=c(k1=k1, k2=k2, k3=k3),
      C0=c(100,500,1000),
      F0_Delta14C=c(0,0,0),
      In=LitterInput,
      a21=0.2*k1,
      a12=0.2*k2,
      a32=0.1*k2,
      a23=0.2*k3,
      FcAtm=C14Atm_NH
    )
    F.R14m=getF14R(Feedback)
    F.C14m=getF14C(Feedback)
    F.C14t=getF14(Feedback)
    
    Series=ThreepSeriesModel14(
      t=years,
      ks=c(k1=1/2, k2=1/35, k3=1/100),
      C0=c(100,500,1000),
      F0_Delta14C=c(0,0,0),
      In=LitterInput,
      a21=0.2*k1,
      a32=0.1*k2,
      FcAtm=C14Atm_NH
    )
    S.R14m=getF14R(Series)
    S.C14m=getF14C(Series)
    S.C14t=getF14(Series)
    
    Parallel=ThreepParallelModel14(
      t=years,
      ks=c(k1=1/2, k2=1/35, k3=1/100),
      C0=c(100,500,1000),
      F0_Delta14C=c(0,0,0),
      In=LitterInput,
      gam1=0.6,
      gam2=0.2,
      FcAtm=C14Atm_NH,
      lag=2
    )
    P.R14m=getF14R(Parallel)
    P.C14m=getF14C(Parallel)
    P.C14t=getF14(Parallel)
    
    par(mfrow=c(3,2))
    plot(
      C14Atm_NH,
      type="l",
      xlab="Year",
      ylab=expression(paste(Delta^14,"C ","(\u2030)")),
      xlim=c(1940,2010)
    ) 
    lines(years, P.C14t[,1], col=4)
    lines(years, P.C14t[,2],col=4,lwd=2)
    lines(years, P.C14t[,3],col=4,lwd=3)
    legend(
      "topright",
      c("Atmosphere", "Pool 1", "Pool 2", "Pool 3"),
      lty=rep(1,4),
      col=c(1,4,4,4),
      lwd=c(1,1,2,3),
      bty="n"
    )
    
    plot(C14Atm_NH,type="l",xlab="Year",
         ylab=expression(paste(Delta^14,"C ","(\u2030)")),xlim=c(1940,2010)) 
    lines(years,P.C14m,col=4)
    lines(years,P.R14m,col=2)
    legend("topright",c("Atmosphere","Bulk SOM", "Respired C"),
           lty=c(1,1,1), col=c(1,4,2),bty="n")
    
    plot(C14Atm_NH,type="l",xlab="Year",
         ylab=expression(paste(Delta^14,"C ","(\u2030)")),xlim=c(1940,2010)) 
    lines(years, S.C14t[,1], col=4)
    lines(years, S.C14t[,2],col=4,lwd=2)
    lines(years, S.C14t[,3],col=4,lwd=3)
    legend("topright",c("Atmosphere", "Pool 1", "Pool 2", "Pool 3"),
           lty=rep(1,4),col=c(1,4,4,4),lwd=c(1,1,2,3),bty="n")
    
    plot(C14Atm_NH,type="l",xlab="Year",
         ylab=expression(paste(Delta^14,"C ","(\u2030)")),xlim=c(1940,2010)) 
    lines(years,S.C14m,col=4)
    lines(years,S.R14m,col=2)
    legend("topright",c("Atmosphere","Bulk SOM", "Respired C"),
           lty=c(1,1,1), col=c(1,4,2),bty="n")

    plot(C14Atm_NH,type="l",xlab="Year",
         ylab=expression(paste(Delta^14,"C ","(\u2030)")),xlim=c(1940,2010)) 
    lines(years, F.C14t[,1], col=4)
    lines(years, F.C14t[,2],col=4,lwd=2)
    lines(years, F.C14t[,3],col=4,lwd=3)
    legend("topright",c("Atmosphere", "Pool 1", "Pool 2", "Pool 3"),
           lty=rep(1,4),col=c(1,4,4,4),lwd=c(1,1,2,3),bty="n")
    
    plot(C14Atm_NH,type="l",xlab="Year",
         ylab=expression(paste(Delta^14,"C ","(\u2030)")),xlim=c(1940,2010)) 
    lines(years,F.C14m,col=4)
    lines(years,F.R14m,col=2)
    legend("topright",c("Atmosphere","Bulk SOM", "Respired C"),
           lty=c(1,1,1), col=c(1,4,2),bty="n")
    
    
    par(mfrow=c(1,1))
  }
  )
