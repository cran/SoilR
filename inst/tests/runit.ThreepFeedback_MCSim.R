#
# vim:set ff=unix expandtab ts=2 sw=2:
test.MC=function(){
  # create the operator for a three pool feedback model
  # according to our new general definition
  # 
  t_start=0
  t_end=2
  tn=3e2
  timestep=(t_end-t_start)/tn
  t=seq(t_start,t_end,timestep)
  nr=3
  # define the transfer functions for the model
  # we could compile them to a matrix valued
  # Function of C and t since they will be 
  # applied in a linear way on the output vector.
  # but we rather store them in an indexed list 
  # (as a sparse matrix) which also has some 
  # implementational benefits because the single
  # functions are easier to retrieve from the operator
  # if needed.
  alpha=list()
  #alpha[["2_to_1"]]=function(C,t){
  #  1/5#*1e-16
  #}
  #alpha[["3_to_2"]]=function(C,t){
  #  1/5#*1e-16
  #}
  alpha[["1_to_2"]]=function(C,t){
    4/5#*1e-16
  }
  alpha[["2_to_3"]]=function(C,t){
    4/5#*1e-16
  }


  k1=-3/5
  k2=-3/5
  k3=-3/5
  f=function(C,t){
    force(C)
    force(t)
    # in this case the application of f can be expressed by a matrix multiplication
    # f(C,t)=N C
    # furthermorde the matrix N is actually completely linear and even constant
    N=matrix( 
       nrow=nr,
       ncol=nr,
       c(
          k1,    0,     0,  
          0  ,  k2,     0,  
          0,     0,    k3 
       )
    )
    # so we can write f(C,t)  as a Matrix product
    # note however that we could anything we like with the components
    # of C here. 
    # The only thing to take care of is that we release a vector of the same
    # size as C
    return(N%*%C)
  }
  fac=1e2
  
  inputrates=new("TimeMap",t_start,t_end,function(t){return(matrix(
    nrow=nr,
    rep(
      c(
        2*fac,  0*fac,  0*fac
      ),
      length(t)
    )
  ))})
  A=new("TransportDecompositionOperator",t_start,Inf,nr,alpha,f)
  mod=GeneralNlModel(
   t,
   A,
   c(
      fac,
      fac,
      fac
   ),
  inputrates,
  deSolve.lsoda.wrapper
  )
  # Now we first solve  the solution Ode with our standard solver
  # we don't have to do this with the particle simulation
  # we rather use the results there
	
  MCSim=getParticleMonteCarloSimulator(mod)
  aPP=availableParticleProperties(MCSim)
  aPS=availableParticleSets(MCSim)
  ref_PP=c("t_entrySystem","t_entryPool_1","t_entryPool_2","t_entryPool_3","t_exitSystem")
  ref_PS=c(
  "particles_in_pool_1",
  "particles_in_pool_2",
  "particles_in_pool_3",
  "particles_leaving_pool_1",
  "particles_leaving_pool_2",
  "particles_leaving_pool_3",
  "particles_leaving_the_system"
)
  checkEquals(aPP,ref_PP)
  checkEquals(aPS,ref_PS)
  tasklist=list()
  #tasklist[["meanTransitTime"]] <- quote(
  #              mean(
  #                 particleSets[["particles_leaving_the_system"]][,"t_exitSystem"]
  #                -particleSets[["particles_leaving_the_system"]][,"t_entrySystem"]
  #              )
  #)
  tasklist[["Cstock_1"]] <- quote(nrow(particleSets[["particles_in_pool_1"]]))
  tasklist[["Cstock_2"]] <- quote(nrow(particleSets[["particles_in_pool_2"]]))
  tasklist[["Cstock_3"]] <- quote(nrow(particleSets[["particles_in_pool_3"]]))

  MCSim[["tasklist"]]<-tasklist
  plot(MCSim)
}

