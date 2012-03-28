##########################################################
setClass(# decomposition operator 
    Class="DecompositionOperator",
    contains="TimeMap"     
   )
setGeneric ( # This function 
   name= "getTransitTimeDistributionDensity",
   def=function(# access to the transit time distribution 
      ### This function computes the transit time distribution density  for a given Decomposition Operator
      ### by computing the output (respiration) of the system according to an instantaneous input.
      
      
      ##details<<Let \eqn{O(t)} describe the overall output  flux of the system.
      ## One can imagine  \eqn{O(t)} as a weighted sum of previous inputs \eqn{I(t-T)}. The weights for 
      ## this summation are
      ## described by the transit time distribution density \eqn{\psi(T)} where \eqn{T} is the 
      ## transit time, the time from entry to exit of a particle.
      ## This is expressed by the following relation:
      ##\eqn{O(t)=\int_0^\infty \psi(T) I(t-T) dT}
      ## For an instantaneous input \eqn{I(t-T)=\delta(t-T)} the transit time distribution density becomes
      ## equal to the output of the system, the respiration rate. 
      ## we can therefore compute the transfer time distribution by applying our Decomposition Operator to an 
      ## instantaneous imput, which is realized by an inputrate of zero and startvalues of one.
     
               
                
      ##references<< 1.) <ManzoniJGR>
                object,### a DecompositionOperator Object 
                inputDistribution,### a vector of length equal to the number of pools. The entries are weights. That means that the 
                                  ### to one!.
                times ### the times for which the distribution density is sought
	){standardGeneric("getTransitTimeDistributionDensity")}
)
setMethod(
   f= "getTransitTimeDistributionDensity",
      signature= "DecompositionOperator",
      definition=function(object,inputDistribution,times){
      # we set the initial values to the value provided by the inputdistribution
      sVmat=inputDistribution
      n=length(inputDistribution)
      # we provide a zero imputflux
      inputFluxes=new(
        "TimeMap",
        -Inf,
        +Inf,
        function(t0){matrix(nrow=n,ncol=1,c(0))}
      ) 
      #we create a model 
      mod=GeneralModel(times,object,sVmat,inputFluxes)
      R=getReleaseFlux(mod)
      TTD=rowSums(R)
      return(TTD)
   }
)
setGeneric ( # This function 
   name= "getMeanTransitTime",
   def=function(# access to the mean transit time 
      ### This function computes the Expected Value of the transit time 
      ### by integrating the product of t and the TransitTimeDistributionDensity

      object,           ### a DecompositionOperator Object 
      inputDistribution ### a vector of length equal to the number of pools. The entries are weights. 
                        ### which sum to 1
	){standardGeneric("getMeanTransitTime")}
)
setMethod(
   f= "getMeanTransitTime",
      signature= "DecompositionOperator",
      definition=function(object,inputDistribution){

      # we create function that receives a vector of times 
      # computes the TransitTimeDistribution at these points 
      # which will be choosen by the integrate function 
      # then we multiply it with t
      integrand <- function(times){
        # we set the initial values to the value provided by the inputdistribution
        o=order(times)
        ot=times[o]
        ttd=getTransitTimeDistributionDensity(object,inputDistribution,ot)
        ores=(ttd*ot) 
        #to invert the permutation we compute the permutation of the permutation 
        oo=order(o)
        res=ores[oo]
        #res=ttd[oo]
        return(res)
      }
      #t_end=23.9
      #pdf(file="meantest.pdf",paper="a4")
      #t_step=t_end/10000
      #t=seq(0,t_end,t_step)
      #  plot(t,integrand(t),type="l",lty=2,col=1,ylab="Concentrations",xlab="Time")
      #dev.off()
      
      #The integrate function of R does simply not work precisely in this example
      #meanTime=integrate(integrand,0,Inf,subdivisions=1000000)[["value"]] 
      #we therefor build a (crude) replacement using the fact that the transit time distriution density 
      #will vanish for large values
      #what large means actually depends on the matrix and has to be checked
      # up to now we only use a very crude estimate of 2000 years
      # note that this large value must still be in the time range where the Operator is defined
      t_end=2000
      t_step=t_end/100000
      t=seq(0,t_end,t_step)
      
      ttdd=getTransitTimeDistributionDensity(object,inputDistribution,t)
      meanTime=sum(ttdd*t)*t_step
      print(meanTime)
      return(meanTime)
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
##########################################################
