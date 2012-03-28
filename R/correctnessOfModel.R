correctnessOfModel=function
### The parameters used by the function \code{\link{GeneralModel}} in SoilR have a biological meaning, and therefore cannot be arbitrary.
### This functions tests some of the obvious constraints of the general model. 
### Up to now these are:
### 1) The compatibility of the decomposition rates and the transport parameters to and from other pools, i.e. 
### the column-wise sum of the elements cannot be negative. Otherwise this would create negative values of respiration, which are not biologically meaningful.
### 2) The compatibility of the time ranges of the supplied functions 

(times,			##<< A vector containing the points in time where the solution is sought. 

 Atm,			##<< An object of class TimeMap containing a matrix valued function and the time range where it is applicable. The matrix function contains the whole model decomposition rates , connection and feedback coefficients for the n pools. The size of this matrix is equal to the number of pools.
 InputFluxes	##<< An object of class TimeMap containing a vecotor valued function and the time range where it is applicable. The vector function contains the whole model inputFluxes for the n pools. The size of this vector is therefor equal to the number of pools.
)
{   A=Atm@map
    #compute the respiration coefficients as funtions of time
    rcoeffs=RespirationCoefficients(A)
    r=sapply(times,rcoeffs)
    #mark the negative respirations (which will trigger the refusal of the matrix )
    truthv=sapply(r,is.negative)
    #find the bad columns 
    positions=grep("TRUE",truthv)
    res=TRUE
    if (length(positions)>0){
       print("The following columns contain unreasonable entries that lead to negative respirations for these pools. Please check your matrix as function of time.")
        res=FALSE}
     
    tA_min=getTimeRange(Atm)["t_min"]
    tA_max=getTimeRange(Atm)["t_max"]
    tI_min=getTimeRange(InputFluxes)["t_min"]
    tI_max=getTimeRange(InputFluxes)["t_max"]
    t_min=min(times)
    t_max=max(times)
    if (t_min<tA_min) {
        print("You ordered a timeinterval that starts earlier than the interval your matrix valued function A(t) is defined for. \n Have look at the timeMap object of A(t) or the data it is created from"
        )
        res=FALSE
    }
    if (t_max>tA_max) {
        print("You ordered a timeinterval that ends later than the interval your matrix valued function A(t) is defined for. \n Have look at the timeMap object of A(t) or the data it is created from"
        )
        res=FALSE
    }
    if (t_min<tI_min) {
        print("You ordered a timeinterval that starts earlier than the interval your function I(t) (InputFluxes) is defined for. \n Have look at the timeMap object of I(t) or the data it is created from"
        )
        res=FALSE
    }
    if (t_max>tI_max) {
        print("You ordered a timeinterval that ends later than the interval your function I(t) (InputFluxes) is defined for. \n Have look at the timeMap object of I(t) or the data it is created from"
        )
        res=FALSE
    }

    return(res)
}
is.negative=function(number){
   ### the function returns True if the argumente is negative
   return(number<0)
}
