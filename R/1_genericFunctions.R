#
# vim:set ff=unix expandtab ts=2 sw=2:
setGeneric(
    name="Delta14C",
    def=function( # convert its argument to a Delta14C representation
    ### Thfunction returns an object of the same type as its imput
    ### this can be a number a matrix or an object of class FcAtm
    F ##<< an object that contains data and a formatdescription.  So it can be converted into the AbsoluteFractionModern format if a conversion is implemented.
    ){
        standardGeneric("Delta14C")
    }
)
setGeneric(
    name="Delta14C_from_AbsoluteFractionModern",
    def=function( # convert its argument from an Absolute Fraction Modern to a Delta14C representation
    ### The function returns an object of the same type as its imput
    ### this can be a number a matrix 
    AbsoluteFractionModern ##<< an object
    ){
        standardGeneric("Delta14C_from_AbsoluteFractionModern")
    }
)
setGeneric(
    name="AbsoluteFractionModern",
    def=function( # convert its argument to an Absolute Fraction Modern representation
    ### The function returns an object of the same type as its imput
    ### this can be a number a matrix or an object of class FcAtm
    F ##<< an object that contains data and a formatdescription.  So it can be converted into the AbsoluteFractionModern format if a conversion is implemented.
    ){
        standardGeneric("AbsoluteFractionModern")
    }
)
setGeneric(
    name="AbsoluteFractionModern_from_Delta14C",
    def=function( # convert its argument to an Absolute Fraction Modern representation
    ### The function returns an object of the same type as its imput
    ### this can be a number a matrix 
    delta14C){
        standardGeneric("AbsoluteFractionModern_from_Delta14C")
    }
)
setGeneric(
    name="getFormat",
    def=function( # extract the format from an object that contains one
    ### The function returns a format string that describes the format the data is given in
    ### The more detailed information is to be found in the methods		 
    object ##<< usually an object of a subclass of TimeMap
    ){
        standardGeneric("getFormat")
    }
)
setGeneric(
    name="getValues",
    def=function( # extract numeric values from an object that contains additional informations like format or even units
    ### The function returns the actaul  number(s) 
    ### The more detailed information is to be found in the methods		 
    object ##<< usually an object of a class implemented by soilR e.g. a startvalue for the 14C fraction. 
    ){
        standardGeneric("getValues")
    }
)
setMethod(
   f= "AbsoluteFractionModern_from_Delta14C",
      signature("numeric"),
      definition=function(# convert from Delta14C to Absolute Fraction Normal values  
	delta14C ##<< numeric containing the values in Delta14C format
	){
	### convert a number matrix of vector containing Delta14C values to the appropriate Absolute Fraction Modern values .
	fprime=(delta14C/1000)+1
	return(fprime)
	}
)
setMethod(
   f= "Delta14C_from_AbsoluteFractionModern",
      signature("numeric"),
      definition=function(# convert Absolute Fraction Normal values to Delta14C values 
	AbsoluteFractionModern ##<< numeric containing the values in Absolute Fraction Modern format
	){
	D14C=(AbsoluteFractionModern-1)*1000
	return(D14C)
	}
)
setMethod(
   f= "AbsoluteFractionModern_from_Delta14C",
      signature("matrix"),
      definition=function(# convert from Delta14C to Absolute Fraction Normal values  
	delta14C ##<< numeric containing the values in Delta14C format
	){
	### convert a number matrix of vector containing Delta14C values to the appropriate Absolute Fraction Modern values .
	fprime=matrix(
	    nrow=nrow(delta14C),
	    ncol=ncol(delta14C),
	    sapply(delta14C,AbsoluteFractionModern_from_Delta14C)
	)
	return(fprime)
	}
)
setMethod(
   f= "Delta14C_from_AbsoluteFractionModern",
      signature("matrix"),
      definition=function(# convert Absolute Fraction Normal values to Delta14C values 
	AbsoluteFractionModern ##<< numeric containing the values in Absolute Fraction Modern format
	){
	D14C=matrix(
	    nrow=nrow(AbsoluteFractionModern),
	    ncol=ncol(AbsoluteFractionModern),
	    sapply(AbsoluteFractionModern,Delta14C_from_AbsoluteFractionModern)
	)
	return(D14C)
	}
)
setGeneric ( # This function 
   name= "getMeanTransitTime",
   def=function(# Access to the mean transit time 
      ### This function computes the Expected Value of the transit time 
      ### by integrating the product of t and the TransitTimeDistributionDensity

     ##references<< Manzoni, S., G.G. Katul, and A. Porporato. 2009. Analysis of soil carbon transit times and age distributions using network theories.
     ## Journal of Geophysical Research-Biogeosciences 114, DOI: 10.1029/2009JG001070.
     
      object,           ##<< a DecompositionOperator Object. 
      inputDistribution ##<< a vector of length equal to the number of pools. The entries are weights, which must sum to 1.
      
	){standardGeneric("getMeanTransitTime")}
)
setGeneric ( # This function 
   name= "getTransitTimeDistributionDensity",
   def=function(# Access to the transit time distribution 
      ### This function computes the transit time distribution density  for a given time Decomposition Operator if it is time invariant 
      ### by computing the output (respiration) of the system as a function of time according to an instantaneous input.
      
      
      ##details<<
      ## The computation is based on the following assumptions which are requirements of the function and checked:
      ## Let \eqn{O(t)} describe the overall output  flux of the system.
      ## One can imagine  \eqn{O(t)} as a weighted sum of previous inputs \eqn{I(t-T)}. The weights for 
      ## this summation are
      ## described by the transit time distribution density \eqn{\psi(T,t)} where \eqn{T} is the 
      ## transit time, the time from entry to exit of a particle and \eqn{t} the time of observation.
      ## This is expressed by the following relation:
      ##\eqn{O(t)=\int_0^\infty \psi(T,t) I(t-T) dT} which is universally true since everything coming out of the system must have entered at some point.
      ## In the case of a time invariant decomposition operator and a time invariant input rate \eqn{I(t-T)} the system will eventually reach a stady state where in and outflow are balanced. In this case (which Erikson calles "stationary state" ) also the transit time distribution becomes time invariant: \eqn{\psi(T,t)=\psi(T)}.

      ## For an instantaneous input \eqn{I(t-T)=\delta(t-T)} the transit time distribution density becomes
      ## equal to the output of the system, the release rate. 
      ## we can therefore compute the transfer time distribution by applying our Decomposition Operator to an 
      ## instantaneous input, which is realized by an inputrate of zero and startvalues of one.
     
               
                
      ##references<< Manzoni, S., G.G. Katul, and A. Porporato. 2009. Analysis of soil carbon transit times and age distributions using network theories.
                     ## Journal of Geophysical Research-Biogeosciences 114, DOI: 10.1029/2009JG001070.
                object, ##<< a protoDecompositionOperator Object 
                inputDistribution, ##<< a vector of length equal to the number of pools. The entries are weights. That means that their sume must be equal to one!.
                times ##<< the times for which the distribution density is sought
	){standardGeneric("getTransitTimeDistributionDensity")}
)
setGeneric (
   name= "getTimes",
   def=function(#extract the times argumen
	### This functions extracts the times argument from an argument of class NlModel
   object){standardGeneric("getTimes")}
)
setGeneric (
   name= "getInitialValues",
   def=function(#extract the times argumen
	### This functions extracts the times argument from an argument of class NlModel
   object){standardGeneric("getInitialValues")}
)

setGeneric ( # This function 
   name= "getOutputFluxes",
   def=function# complete output of all pools, including the part transfered to other pools.
      ### This functions computes the output flux for all pools. Note that for any given pool not all the output of the pool is released from the system because it migtht as well be fed into other pools. If you are interested what a pool releases from the system use the method \code{\link{getReleaseFlux}}, which internally makes use of this method but afterwards substracts all parts of the outputs  that are fed to other pools.
   (
	object ##<< An object of class Model or Model14 created by a call to \code{\link{GeneralModel}} or other model creating functions.
  ,as.closures=F ##<< if set to TRUE instead of a matrix a list of functions will be returned.  
	){standardGeneric("getOutputFluxes")
    ##value<< A matrix with m columns representing the number of pools, and n rows representing the time step as specified by the argument
    ##\code{t} in \code{\link{GeneralModel}} or other model creating function.
    ##details<< This function takes a Model object, which represents a system of ODEs of the form 
    ##\deqn{\frac{d \mathbf{C}(t)}{dt} = \mathbf{I}(t) + \mathbf{A}(t) \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t)} 
    ##and solves the system for \eqn{\mathbf{C}(t)}{C(t)}. The numerical solver used can be specified in \code{\link{GeneralModel}}.
	  ##seealso<< See examples in \code{\link{GeneralModel}}, \code{\link{GeneralModel_14}}, \code{\link{TwopParallelModel}}, 
    ## \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}}, etc.
   }
   
)
setGeneric ( # This function 
   name= "getC",
   def=function(# Calculates the C content of the pools 
    ### This function computes the carbon content of the pools as function of time
	object ##<< An object of class Model or Model14 created by a call to \code{\link{GeneralModel}} or other model creating functions.
  ,as.closures=F ##<< if set to TRUE instead of a matrix a list of functions will be returned.  
	){standardGeneric("getC")
    ##value<< A matrix with m columns representing the number of pools, and n rows representing the time step as specified by the argument
    ##\code{t} in \code{\link{GeneralModel}} or other model creating function.
    ##details<< This function takes a Model object, which represents a system of ODEs of the form 
    ##\deqn{\frac{d \mathbf{C}(t)}{dt} = \mathbf{I}(t) + \mathbf{A}(t) \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t)} 
    ##and solves the system for \eqn{\mathbf{C}(t)}{C(t)}. The numerical solver used can be specified in \code{\link{GeneralModel}}.
	  ##seealso<< See examples in \code{\link{GeneralModel}}, \code{\link{GeneralModel_14}}, \code{\link{TwopParallelModel}}, 
    ## \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}}, etc.
   }
   
)
setGeneric ( # This function 
   name= "getParticleMonteCarloSimulator",
   def=function(# creates an MCSimulator that can be run afterwards to get statistical estimates for mean transit times and ages either pool specific of 
   ### This function computes carbon release from each pool of the given model as funtion of time 
	object ##<< An object of class NlModel ,NlModel14 or their subclasses created by a call to \code{\link{NlModel}} or other model creating functions.
	){standardGeneric("getParticleMonteCarloSimulator")
	  ##value<< MCS a Monte Carlos Simulator object (basically a closure) that can be run with various input parameters
    }
)
setGeneric ( # This function 
   name= "getReleaseFlux",
   def=function(# Calculates the release of C from each pool
   ### This function computes carbon release from each pool of the given model as funtion of time 
	object ##<< An object of class Model or Model14 created by a call to \code{\link{GeneralModel}} or other model creating functions.
	){standardGeneric("getReleaseFlux")
	  ##value<< A n x m matrix of release fluxes with m columns representing the number of pools, and n rows representing the time step as specified by the argument
	  ##\code{t} in \code{\link{GeneralModel}} or other model creating function.
	  ##details<< This function takes a Model object, which represents a system of ODEs of the form 
	  ##\deqn{\frac{d \mathbf{C}(t)}{dt} = \mathbf{I}(t) + \mathbf{A}(t) \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t)} 
	  ##solves the system for \eqn{\mathbf{C}(t)}{C(t)}, calculates a diagonal matrix of release coefficients \eqn{\mathbf{R}(t)}{R(t)}, 
    ##and computes the release flux as \eqn{\mathbf{R}(t) \mathbf{C}(t)}{R(t) C(t)}.
    ##The numerical solver used can be specified in \code{\link{GeneralModel}}.
	  ##seealso<< See examples in \code{\link{GeneralModel}}, \code{\link{GeneralModel_14}}, \code{\link{TwopParallelModel}}, 
	  ## \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}}, etc.
   }
)
setGeneric ( 
   name= "getAccumulatedRelease",
   def=function(# Calculates the accumulated carbon release from the pools as a function of time
   ### This function computes the accumulated carbon release of the given model as funtion of time 
	object ##<< An object of class Model or Model14 created by a call to \code{\link{GeneralModel}} or other model creating functions.
	){standardGeneric("getAccumulatedRelease")
	  ##value<< A n x m matrix of cummulative release fluxes with m columns representing the number of pools, and n rows representing the time step as specified by the argument
	  ##\code{t} in \code{\link{GeneralModel}} or other model creating function.
	  ##details<< This function takes a Model object, calculates the release flux as specified by \code{\link{getReleaseFlux}}, 
    ##and integrates numerically the release flux up to each time step \code{t}.
	  ##seealso<< See examples in \code{\link{GeneralModel}}, \code{\link{GeneralModel_14}}, \code{\link{TwopParallelModel}}, 
	  ## \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}}, etc.
	 }
)
setGeneric ( # This function 
   name= "getC14",
   def=function(# access to the C content of the pools 
    ### This function computes the value for C (mass or concentration ) as function of time
	object
	){standardGeneric("getC14")}
)
setGeneric ( # compute  \eqn{\frac{^{14}C}{C}}{14C/C} ratio 
   name= "getF14",
   def=function(# access to the C content of the pools 
   ### This function computes the \eqn{\frac{^{14}C}{C}}{14C/C} ratio in the soil as funtion of time 
	object
	){standardGeneric("getF14")}
)
setGeneric ( # This function 
   name= "getReleaseFlux14",
   def=function(# access to the C content of the pools 
   ### This function computes the \eqn{^{14}C}{14C} release of the given model as funtion of time 
	object
	){standardGeneric("getReleaseFlux14")}
)
setGeneric ( # This function 
  name= "getF14R",
  def=function(# access to the C release flux from the pools 
    ### This function computes the \eqn{\frac{^{14}C}{C}}{14C/C} ratio of the released C as funtion of time 
    object
    ){standardGeneric("getF14R")}
  )
setGeneric ( # This function 
  name= "getF14C",
  def=function(# access to the C release flux from the pools 
    ### This function computes the \eqn{\frac{^{14}C}{C}}{14C/C} ratio of the released C as funtion of time 
    object
    ){standardGeneric("getF14C")}
  )
setGeneric(
    name="getTimeRange",
    def=function(object){
    ### The function returns the time range of the given TimeMap object. 
        standardGeneric("getTimeRange")
    }
)
setGeneric(
    name="getFunctionDefinition",
    def=function(object){
    ### extract the function definition (the R-function) from the argument
        standardGeneric("getFunctionDefinition")
    }
)
setGeneric(
    name="getNumberOfPools",
    def=function(object){
    ### extract the function definition (the R-function) from the argument
        standardGeneric("getNumberOfPools")
    }
)
setGeneric(
    name="getOutputReceivers",
    def=function(object,i){
    ### extract the InputFluxes from a model object
        standardGeneric("getOutputReceivers")
    }
)
setGeneric(
    name="getDecompositionOperator",
    def=function(object){
    ### extract the InputFluxes from a model object
        standardGeneric("getDecompositionOperator")
    }
)
setGeneric(
    name="getInputFluxes",
    def=function(object){
    ### extract the InputFluxes from a model object
        standardGeneric("getInputFluxes")
    }
)
setGeneric(
    name="availableParticleProperties",
    def=function(object){
    ### show the variables available for every particle in every timestep
        standardGeneric("availableParticleProperties")
    }
)
setGeneric(
    name="availableParticleSets",
    def=function(object){
    ### show the particle sets available for computation
        standardGeneric("availableParticleSets")
    }
)
setGeneric(
    name="computeResults",
    def=function(object){
        standardGeneric("computeResults")
    }
)
setGeneric(
    name="getDotOut",
    def=function(object){
        standardGeneric("getDotOut")
    }
)
setGeneric(
    name="getTransferMatrix",
    def=function(object){
        standardGeneric("getTransferMatrix")
    }
)
setGeneric(
    name="getTransferCoefficients",
    def=function(object){
        standardGeneric("getTransferCoefficients")
    }
)
setGeneric(
    name="getTransferCoefficients",
    def=function(object,as.closures=F){
        standardGeneric("getTransferCoefficients")
    }
)
