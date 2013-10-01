#
# vim:set ff=unix expandtab ts=2 sw=2:
correctnessOfFcAtm=function#check for unreasonable parameters or unsupported formats
###  The atmospheric C14 data can be represented in more than one format 
###  The function checks if the user required format is supported at the moment
(object ##<< the object to be tested
)
{
   res=TRUE
   supported_formats=supported14CFractionFormats()
   f=object@format
   print(paste("format=",f))
   if (!any(grepl(f,supported_formats))){
      err_str=cat("The required format:",f," describing the atmospheric c_14 fraction is not supported.\n 
   	     The supported formats are: ",supported_formats,". \n",sep="")
      stop(simpleError(err_str))
      return(res)
   }
}

##############################################################################################
setClass(# A special form of TimeMap containing the atmospheric C14 fraction and the format it is provided in. 
    Class="FcAtm",
    contains="TimeMap",
    representation=representation(
        format="character"
    )
    #,validity=correctnessOfFcAtm #set the validating function
)
setMethod(
    f="initialize",
    signature="FcAtm",
    definition=function(.Object,starttime=numeric(),endtime=numeric(),map=function(t){t},lag=0,format="Delta14C"){
    #cat("-initializer at work-\n")
    .Object@starttime=starttime
    .Object@endtime=endtime
    .Object@map=map
    .Object@lag=lag
    .Object@format=format
    correctnessOfFcAtm(.Object)
    return(.Object)
    }
)
setMethod(
    f="getFormat",
    signature="FcAtm",
    definition=function(# extract the format string
			object ##<< object of class FcAtm a subclass of TimeMap containing imformation about the format that could be Delta14C or AFM (Absolute Fraction Modern) for instance
			){
       ### the function just yields the format as a string
        return(object@format)
    }
)
setMethod(
   f= "Delta14C",
      signature("FcAtm"),
      definition=function(# convert to Absolute Fraction Normal values  
	F ##<< object of class FcAtm containing the values in any format
	){
	### convert a FcAtm object containing values in any supported format to the appropriate Absolute Fraction Modern values.
	f=F@format
        targetFormat="Delta14C"
        if (f==targetFormat){
	   # do nothing
	   return(F)
	}
	if (f=="AbsoluteFractionModern"){
	 f_afn=F@map
         f_d14C=function(t){
	     fd=Delta14C_from_AbsoluteFractionModern(f_afn(t))
	 return(fd)
	 }
	 D14C=F
	 D14C@map=f_d14C
	 D14C@format=targetFormat
	 return(D14C)
	} 
      stop("conversion not implemented for this format")
      }	 
)
setMethod(
   f= "AbsoluteFractionModern",
      signature("FcAtm"),
      definition=function(# convert to Absolute Fraction Normal values  
	F ##<< object of class FcAtm containing the values in any format
	){
	### convert a FcAtm object containing values in any supported format to the appropriate Absolute Fraction Modern values.
	f=F@format
        targetFormat="AbsoluteFractionModern"
        if (f==targetFormat){
	   # do nothing
	   return(F)
	}
	if (f=="Delta14C"){
	 f_d14C=F@map
         f_afn=function(t){
	     fprime=AbsoluteFractionModern_from_Delta14C(f_d14C(t))
	 return(fprime)
	 }
	 AFM_tm=F
	 AFM_tm@map=f_afn
	 AFM_tm@format=targetFormat
	 return(AFM_tm)
	} 
      stop("conversion not implemented for this format")
      }	 
)

FcAtm.from.Dataframe=function
### This function is another constructor of the class FcAtm
(dframe, ##<<A data frame containing exactly two columns:
## the first one is interpreted as time
## the secon one is interpreted as atmospheric C14 fraction in the format mentioned
lag=0, ##<< a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)
interpolation=splinefun, ##<<A function that  returns a function  the default is splinefun. Other possible values are the linear interpolation approxfun or any self made function with the same interface.
format ##<< a string that specifies the format used to represent the atmospheric fracton. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation 
){
   t=dframe[,1]  
   y=dframe[,2]  
   o=order(t)
   tyo=cbind(t[o],y[o])
   to=tyo[,1]+lag# account for the lag time
   yo=tyo[,2]
   t_start=min(to)
   t_start=min(t)
   t_end=max(t)
   interpol=interpolation(to,yo)
   obj=new(Class="FcAtm",t_start,t_end,interpol,lag=lag,format=format) 
return(obj)
### An object of class TimeMap that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag
}
