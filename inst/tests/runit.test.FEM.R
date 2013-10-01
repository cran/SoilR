#
# vim:set ff=unix expandtab ts=2 sw=2:
#!/usr/bin/Rscript
# vim: set expandtab ts=4
require(RUnit)
require(FME)

HIV_R <-function(pars,V_0=50000,dV_0=-200750,T_0=100){
    derivs <- function(time,y,pars){
        with(as.list(c(pars,y)),{
            dT <- lam-rho*T-bet*T*V
            dI <- bet*T*V-delt*I
            dV <- n*delt*I-c*V - bet*T*V
            return(list(c(dT,dI,dV),logV=log(V)))
        })
    }
    #initial conditions
    I_0 <- with(as.list(pars),(dV_0+c*V_0)/(n*delt))
    y   <- c(T=T_0,I=I_0,V=V_0)
    times   <- c(seq(0,0.8,0.1),seq(2,60,2))
    out     <- ode(y=y,parms=pars,times=times,func=derivs)
    as.data.frame(out)
}
HIVcost <- function(pars){
    out <- HIV_R(pars)
    cost <- modCost(model=out,obs=DataLogV,err="sd")
    return(modCost(model=out,obs=DataT,err="sd",cost=cost))
}
HIVcost2 <- function(lpars){
    HIVcost(c(exp(lpars),n=900))
}
#test.FME=function(){
    #testrun of the model
    pars=c(bet=0.00002,rho=0.15,delt=0.55,c=5.5,lam=80,n=900)
    V_0=50000
    dV_0=-200750
    T_0=100
    out=HIV_R(pars,V_0,dV_0,T_0)
    par(mfrow=c(1,2))
    plot(out[["time"]],out[["logV"]],main="Viral load",ylab="log(V)",xlab="time",type="b")
    plot(out[["time"]],out[["T"]],main="CD4+ T",ylab="-",xlab="time",type="b")
    par(mfrow=c(1,1))

    #create testdata
    std=0.45
    DataLogV <- cbind(
        time=out$time,
        logV=out$logV+rnorm(sd=std,n=length(out$logV)),
        sd=std
    )
    std=4.5
    ## create indexset for the more seldom measured data
    ii <- which(out$time %in% seq(0,56,by=4))
    DataT <- cbind(
        time=out$time[ii],
        T=out$T[ii]+rnorm(sd=std,n=length(ii)),
        sd=std
    )
    plot(HIVcost(pars),xlab="time")
                            
    #plot the sensitivity functions
    Sfun <- sensFun(HIVcost,pars)
    plot(Sfun,which=c("logV","T"),xlab="time",lwd=2)
    #plot the pairs
    pairs(Sfun,which=c("logV","T"),col=c("blue","green"))
    ident <- collin(Sfun)
    head(ident,n=20)
    #plot it 
    plot(ident,log="y")
    #show that all parameters togehter are not identifiable
    collin(Sfun,parset=c("bet","rho","delt","c","lam","n"))
    #lets try to find a 5 parameter set that could be identifiable
    collin(Sfun,N=5)
    #but this will not be possible on T alone
    collin(Sfun,parset=c("bet","rho","delt","c","lam"),which="T")
    #and also not on logV alone
    collin(Sfun,parset=c("bet","rho","delt","c","lam"),which="logV")
    #now we fit the model to the data
    Pars <- pars[1:5]*2
    Fit <- modFit(f=HIVcost2,p=log(Pars))
    exp(coef(Fit))
    deviance(Fit)
    ini <- HIV_R(pars=c(Pars,n=900))
    final <- HIV_R(pars=c(exp(coef(Fit)),n=900))
    
#compare the results
    par(mfrow=c(2,1))
    plot(DataLogV,xlab="time",ylab="logV",ylim=c(7,11))
    lines(ini$time,ini$logV,lty=2)
    lines(final$time,final$logV)
    legend("topright",c("data","initial","fitted"),lty=c(NA,2,1),pch=c(1,NA,NA))

    plot(DataT,xlab="time",ylab="T")
    lines(ini$time,ini$T,lty=2)
    lines(final$time,final$T)

    par(mfrow=c(1,1))
#bayesian part
    var0 <- Fit$var_ms_unweighted
    cov0 <- summary(Fit)$cov.scaled*2.4^2/5
    n=500
    t1=Sys.time()
    MCMC  <- modMCMC(f=HIVcost2,p=Fit$par,niter=n,jump=cov0,var0=var0,wvar0=0.1,updatecov=50)
    #backtransform the parameters
    MCMC$pars <- exp(MCMC$pars)
    t2=Sys.time()
    print(t1-t2)
    summary(MCMC)
    #plot(MCMC, Full = TRUE)
    #pairs(MCMC, nsample = n/2)
#}  


