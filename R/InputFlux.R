#
# vim:set ff=unix expandtab ts=2 sw=2:
##########################################################
setClass(# decomposition operator 
    Class="InputFlux",
    contains="TimeMap"     
   )
setMethod(
      f="initialize",
      ### 
      signature="InputFlux",
      definition=function(.Object,starttime,endtime,map){
        #print("this is the initialize method for the class InputFlux We can put tests here to
        #      check if the arguments are valid")
        Object <- callNextMethod(.Object,starttime,endtime,map)
     }
)
