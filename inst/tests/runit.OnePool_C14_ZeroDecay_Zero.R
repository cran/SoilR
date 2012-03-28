test.OnePool_C14_ZeroDecay_Zero=function(){
   require(SoilR)
   require(RUnit)
   t_start=0
   t_end=50000
   tn=100
   timestep=(t_end-t_start)/tn
   t=seq(t_start,t_end,timestep)
   #begin analytical solutions
   c01=0.5
   th=5730
   k=log(0.5)/th
   Y=matrix(ncol=1,nrow=length(t))
   Y[,1]=c01*exp(k*t)
   tol=.02*max(Y)/tn
   print(tol)
   R=matrix(ncol=1,nrow=length(t))
   R[,1]=0
   #end analytical solutions
   A=TimeMap.new(t_start,t_end,function(t){matrix(
     nrow=1,
     ncol=1,
     c(
        0
     )
   )})
   inputrates=TimeMap.new(t_start,t_end,function(t){return(matrix(
     nrow=1,
     ncol=1,
     c(
        0
     )
   ))})
   Fc=TimeMap.new(t_start,t_end,function(t){500})
   mod=GeneralModel_14(
    t,
    A,
    c(
       c01
    ),
   inputrates,
   Fc,
   k,
   deSolve.lsoda.wrapper
   )
   Yode=getC14(mod) 
   Rode=getReleaseFlux(mod) 
# start plots
   lt1=2
   lt2=4
   plot(t,Y[,1],type="l",lty=lt1,col=1,ylab="Concentrations",xlab="Time")
   lines(t,Yode[,1],type="l",lty=lt2,col=1)
   legend(
   "topright",
     c(
     "anylytic sol for pool 1",
     "numeric sol for pool 1"
     ),
     lty=c(lt1,lt2),
     col=c(1,1)
   )
   checkEquals(
    Y,
    Yode,
    "test numeric solution for C-Content computed by the ode mehtod against analytical",
    tolerance = tol,
   )
}
