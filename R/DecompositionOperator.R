#
# vim:set ff=unix expandtab ts=2 sw=2:

setClass(# decomposition operator 
    Class="DecompositionOperator",
    contains="TimeMap"     
   )
setMethod(
   f= "getTransitTimeDistributionDensity",
      signature= "DecompositionOperator",
      definition=function(object,inputDistribution,times){
      # we set the initial values to the value provided by the inputdistribution
      sVmat=inputDistribution
      n=length(inputDistribution)
      # we provide a zero inputflux
      inputFluxes=new(
        "TimeMap",
        -Inf,
        +Inf,
        function(t0){matrix(nrow=n,ncol=1,0)}
      ) 
      #we create a model 
      mod=GeneralModel(times,object,sVmat,inputFluxes)
      R=getReleaseFlux(mod)
      TTD=rowSums(R)
      return(TTD)
   }
)
setMethod(
   f= "getMeanTransitTime",
      signature= "DecompositionOperator",
      definition=function(object,inputDistribution){

      # we create function that receives a vector of times 
      # computes the TransitTimeDistribution at these points 
      # which will be choosen by the integrate function 
      # then we multiply it with t
      #integrand <- function(times){
      #  # we set the initial values to the value provided by the inputdistribution
      #  o=order(times)
      #  ot=times[o]
      #  ttd=getTransitTimeDistributionDensity(object,inputDistribution,ot)
      #  ores=(ttd*ot) 
      #  #to invert the permutation we compute the permutation of the permutation 
      #  oo=order(o)
      #  res=ores[oo]
      #  #res=ttd[oo]
      #  return(res)
      #}

      #t_end=23.9
      #pdf(file="meantest.pdf",paper="a4")
      #t_step=t_end/10000
      #t=seq(0,t_end,t_step)
      #  plot(t,integrand(t),type="l",lty=2,col=1,ylab="Concentrations",xlab="Time")
      #dev.off()
      
      #The integrate function of R does simply not work precisely in this example
      #meanTime=integrate(integrand,0,Inf,subdivisions=1000000)[["value"]] 
      #we therefor build a replacement using the fact that the transit time distriution density 
      #will vanish for large values
      #what large means actually depends on the matrix and has to be estimated
      # note that this large value must still be in the time range where the Operator is defined which is the reason that the domain often has to be set to infinite values

      # we do this iteratively. We start with an estimate of 2000 years
      # then for a number of points in time between 0 and 2000 years 
      # we compute the inverse of the absolute value of the smallest 
      # eigenvalue of the time dependent matrix describing the decomposition.
      # this is done by function spectralNorm.
      # This is a rough estimate for the half life of the whole system.
      # While it is bigger than our start estimate we will have to increase the 
      # length of the time interval.
      # 
      f=getFunctionDefinition(object)
      g=function(t){spectralNorm(f(t))}
      t_max=function(t_end){
          t_step=t_end/10
          t=seq(0,t_end,t_step)
          norms=sapply(t,g)
          tm=100*max(norms)
	  print(paste("tm=",tm))
	  return(tm)
      } 
      t_end=20
      print(paste("t_end=",t_end,sep=""))
      t_end_new=t_max(t_end)
      print(paste("t_end_new_before while=",t_end_new))
      while(t_end_new>t_end){
          print(t_end)
	  t_end=t_end_new
	  t_end_new=t_max(t_end)
      }
      print("after while")
      longTailEstimate=t_end
      subd=10000
      t_step=t_end/subd
      t=seq(0,t_end,t_step)
      shortTailEstimate=min(sapply(t,g))
      
      ttdd=getTransitTimeDistributionDensity(object,inputDistribution,t)
      #print(paste("ttdd=",ttdd))
      #print(paste("t=",t))
      #print(paste("ttdd*t=",ttdd*t))
      meanTimeRiemann=sum(ttdd*t)*t_step
      int2=splinefun(t,ttdd*t)
      meanTimeIntegrate=integrate(int2,0,t_end,subdivisions=subd)[["value"]] 
      print(paste("meanTimeRiemann=",meanTimeRiemann))
      print(paste("meanTimeIntegrate=",meanTimeIntegrate))
      #meanTime_s=integrate(integrand,0,shortTailEstimate,subdivisions=subd)[["value"]] 
      # here we must first check if the two values differ significantly
      #return(meanTimeRiemann)
      return(meanTimeIntegrate)
   }
)
setMethod(
      f="initialize",
      ### 
      signature="DecompositionOperator",
      definition=function(.Object,starttime,endtime,map){
#        print("this is the initialize method for the class DecompositionOperator. We can put tests here to
#              check if the arguments are valid")
        Object <- callNextMethod(.Object,starttime,endtime,map)
     }
)
