# This test function is automatically produced by the python script:/home/mm/SoilR/RPackages/SoilR/pkg/inst/tests/Rexample.py
test.OnePool_C14_equalDecay_ZeroInput=function(){
   require(RUnit)
   t_start=0
   t_end=2
   tn=100
   tol=.02/tn
   print(tol)
   timestep=(t_end-t_start)/tn
   t=seq(t_start,t_end,timestep)
   A=new("DecompositionOperator",t_start,t_end,function(t){matrix(
     nrow=1,
     ncol=1,
     c(
        -log(2)/5730
     )
   )})
   c01=1
   inputrates=new("TimeMap",t_start,t_end,function(t){return(matrix(
     nrow=1,
     ncol=1,
     c(
        0
     )
   ))})
   Fc=new("TimeMap",t_start,t_end,function(t){.500})
   th=5730
   k=log(0.5)/th
   Y=matrix(ncol=1,nrow=length(t))
   Y[,1]=c01*exp(-t*log(2)/5730)
   R=matrix(ncol=1,nrow=length(t))
   R[,1]=c01*exp(-t*log(2)/5730)*log(2)/5730
   Y14=matrix(ncol=1,nrow=length(t))
   Y14[,1]=0.5*c01*exp(-t*log(2)/2865)
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
   Y14ode=getC14(mod) 
   Yode=getC(mod) 
   Rode=getReleaseFlux(mod) 
#begin plots 
   lt1=2
   lt2=4
   pdf(file="runit.automatic.OnePool_C14_equalDecay_ZeroInput.pdf",paper="a4")
   m=matrix(c(1,2),2,1,byrow=TRUE)
   layout(m)
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
   plot(t,R[,1],type="l",lty=lt1,col=1,ylab="Respirationfluxes",xlab="Time",ylim=c(min(R),max(R)))
   lines(t,Rode[,1],type="l",lty=lt2,col=1)
   legend(
   "topright",
     c(
     "anylytic sol for pool 1",
     "numeric sol for pool 1"
     ),
     lty=c(lt1,lt2),
     col=c(1,1)
   )
   dev.off()
# end plots 
   plot(t,Y14[,1],type="l",lty=lt1,col=1,ylab="14C-Concentrations",xlab="Time",ylim=c(min(Y14),max(Y14)))
   lines(t,Y14ode[,1],type="l",lty=lt2,col=1)
# begin checks 
   tol=.02*max(Y14)/tn
   checkEquals(
    Y14,
    Y14ode,
    "test numeric solution for 14C-Content computed by the ode mehtod against analytical",
    tolerance = tol,
   )

 }