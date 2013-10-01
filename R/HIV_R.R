#
# vim:set ff=unix expandtab ts=2 sw=2:
# vim: set expandtab ts=4
HIV_R <-function
### a test function to be put somewhere else soon
(pars,V_0=50000,dV_0=-200750,T_0=100){
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
