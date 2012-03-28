OnepModel<-structure(
    function #Implementation of a one pool model 
    ### This function creates a model for one pool. It is a wrapper for the more general function \code{\link{GeneralModel}}.
     (t,  		##<< A vector containing the points in time where the solution is sought.
      k,	##<< A scalar with the decomposition rate of the pool. 
      C0,	##<< A scalar containing the initial amount of carbon in the pool.
      In,     ##<< A scalar or a data.frame object specifying the amount of litter inputs by time. 
      xi=1,   ##<< A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates. 
      solver=deSolve.lsoda.wrapper  ##<< A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.
    )	
    { 
      t_start=min(t)
      t_end=max(t)
      if(length(k)!=1) stop("k must be a scalar (length == 1)")
      if(length(C0)!=1) stop("initial conditions must be of length = 1")
      C0=c(C0)
      
      if(length(In)==1){
          inputFluxes=TimeMap.new(
            t_start,
            t_end,
            function(t){matrix(nrow=1,ncol=1,In)}
        )
      }
      if(class(In)=="data.frame"){
         x=In[,1]  
         y=In[,2]  
         inputFlux=splinefun(x,y)
          inputFluxes=TimeMap.new(
            t_start,
            t_end,
            function(t){matrix(nrow=1,ncol=1,inputFlux(t))}
          )
        }
      A=-1*abs(matrix(k,1,1))
      
      if(length(xi)==1) fX=function(t){xi}
      if(class(xi)=="data.frame"){
      X=xi[,1]
      Y=xi[,2]
      fX=splinefun(X,Y)
      }
      Af=TimeMap.new(
        t_start,
        t_end,
        function(t){fX(t)*A}
      )
      Mod=GeneralModel(t=t,A=Af,ivList=C0,inputFluxes=inputFluxes)
     return(Mod)
### A Model Object that can be further queried 
      ##seealso<< \code{\link{TwopParallelModel}},\code{\link{TwopFeedbackModel}} 
    }
    ,
    ex=function(){
      t_start=0 
      t_end=10 
      tn=50
      timestep=(t_end-t_start)/tn 
      t=seq(t_start,t_end,timestep) 
      k=0.8
      C0=100
      In = 30
      
    
      Ex=OnepModel(t,k,C0,In)
      Ct=getC(Ex)
      Rt=getReleaseFlux(Ex)
      Rc=getRelease(Ex)
      
      plot(t,Ct,type="l",ylab="Carbon stocks (arbitrary units)",xlab="Time (arbitrary units)",lwd=2) 
      
      plot(t,Rt,type="l",ylab="Carbon released (arbitrary units)",xlab="Time (arbitrary units)",lwd=2) 
      
      plot(t,Rc,type="l",ylab="Cummulative carbon released (arbitrary units)",xlab="Time (arbitrary units)",lwd=2) 

}
)
