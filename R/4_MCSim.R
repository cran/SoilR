#
# vim:set ff=unix expandtab ts=2 sw=2:
setClass(
   Class="MCSim",
   representation=representation(
	 model="NlModel",
   tasklist="list"
   )
)
#################################################
setMethod(
    f="initialize",
    signature="MCSim",
    definition=function(.Object,model=new(Class="NlModel"),tasklist=list()){
    #cat("-initializer at work-\n")
    .Object@model=model
    .Object@tasklist=tasklist
    return(.Object)
    }
)
#################################################
setMethod(
    f="as.character",
    signature="MCSim",
    definition=function#convert Objects of this class to something printable.
    (x, ##<< An object of class MCSim
     ...){
        return(
            paste( class(x), sep="")
        )
    }
)    

#################################################
setMethod(
    f="plot",
    signature="MCSim",
      definition=function# Summary plot of the result of MCSim run
      ### This method plots all the results computed with the Monte Carlo simulation
     (x, ##<< An object of class MCSim
      ...){
      resnames=names(x@tasklist)
      l=computeResults(x)
      
      for (name in resnames){
      print(name)
      print(l[["cr"]][,name])
        plot(
        l[["cr"]][,"time"],
        l[["cr"]][,name],
        xlab="time",
        ylab=name
        )
      }
    }
)
#################################################
setMethod(
    f="availableParticleSets",
    signature="MCSim",
    definition=function(object){
    n=getNumberOfPools(object@model)
    res=c(
      mapply(stockKey,1:n),
      mapply(leaveKey,1:n),
      "particles_leaving_the_system"
      )
    return(res)
    }
)
#################################################
setMethod(
    f="availableParticleProperties",
    signature="MCSim",
    definition=function(object){
    n=getNumberOfPools(object@model)
    #determine the number of transit times that follow from the transfer model
    f=function(i){paste("t_entryPool_",i,sep="")}
    res=c("t_entrySystem",mapply(f,1:n),"t_exitSystem")
    return(res)
    }
)
#################################################
## overload the [[ operator template created by: method.skeleton("[[","MCSim")
setMethod("[[",
    signature(x = "MCSim"),
    function (x, i, j, ...) 
    {
      if (i=="tasklist"){
        value=x@tasklist
        return(value)
      }else{
        stop(paste("MCSim has no property  ",i,".",sep=""))
      } 
    }
)
#################################################
## overload the [[<- operator template created by: method.skeleton("[[<-","MCSim")
setMethod("[[<-",
    signature(x = "MCSim"),
    function (x, i, j, ..., value) 
    {
      if (i=="tasklist"){
        x@tasklist=value
        return(x)
      }else{
        stop(paste("MCSim has no property  ",i,".",sep=""))
      } 
    }

)
#################################################
setMethod(
    f="computeResults",
    signature="MCSim",
    definition=function(object){
    ### This method creates a particle simulator 
            tcn="time"				#name of time column
            extractColumn=function(df,colname){df[,colname]}
            reduce2singledf=function(l){
            	colavg=function(colname){
            		mat=mcmapply(extractColumn,l,MoreArgs=list(colname))
            		rs=function(i){mean(mat[i,])}
            		newcol=mcmapply(rs,1:nrow(mat))
            		return(newcol)
            	}
            	colnames=setdiff(names(l[[1]]),tcn)
            	res=cbind(l[[1]][,tcn],mapply(colavg,colnames))
            	res=as.data.frame(res)
            	names(res)<-names(l[[1]])
            	return(res)
            }
            singleThreadParticleSimulator=function(pseudoarg){
              tasklist=object@tasklist
              resnames=names(tasklist)
              force(resnames)
            	#####################################################################
            	resultline=function(t,resultlist){
            		tf=data.frame(t)
                names(tf) <- "time"
                rd=cbind(tf,as.data.frame(resultlist))
            		return(rd)
            	}
            	#####################################################################
              avp=availableParticleProperties(object)
            	#####################################################################
            	enteringSet=function(t,n,ip){#creates a dataframe containing a new set of particles with the entry time property set to t 
                npd<- as.data.frame(matrix(ncol=length(avp),nrow=n,dimnames=list(c(),avp)))
                npd[,"t_entrySystem"]=rep(t,n)
                npd[,paste("t_entryPool_",ip,sep="")]=rep(t,n)
                # pp("npd",environment())
                # print(names(npd))
            		return(npd)
            	}
            	#####################################################################
              extract=function(Source,p_leave){
            		np=nrow(Source)
            		r <- runif(np,0,1.0)	#random value for each particle between 0 and 1
            		llp1=r<p_leave			#logical vector true for leaving particles
                
                # create dataframe of leaving particles
            		ls=!llp1			#logical vector true for staying particles
            		Destination<- as.data.frame(Source[llp1,])	
            		UpdatedSource <- as.data.frame(Source[ls,])	
                return(list(Destination=Destination,UpdatedSource=UpdatedSource))
              }
            	#####################################################################
              transmit=function(Source,Destination,p_move){
            		np=nrow(Source)
            		r <- runif(np,0,1.0)	#random value for each particle between 0 and 1
            		llp1=r<p_move #logical vector true for leaving particles
                
                # create dataframe of leaving particles
            		ls=!llp1			#logical vector true for staying particles
            		Destination<- rbind(Destination,as.data.frame(Source[llp1,]))	
            		UpdatedSource <- as.data.frame(Source[ls,])	
                return(list(Destination=Destination,UpdatedSource=UpdatedSource))
              }
            	#####################################################################
              mod=object@model
              op=getDecompositionOperator(mod)
              times=getTimes(mod)
            	nt=length(times)			#number of timesteps 
            	st=times[[1]]					#Starttime
              iv=getInitialValues(mod)
              Cs =getC(mod,as.closures=TRUE)
              Os =getOutputFluxes(mod,as.closures=TRUE)
              Tr=getTransferCoefficients(mod,as.closures=TRUE)
              ntr=names(Tr)
              vI=getFunctionDefinition(getInputFluxes(mod))
              
              # initialize dataframes for particle stock and output for every pool
              nop=getNumberOfPools(mod)
              particleSets=list()
              # create a dataframe without content but with the correct columns for the properties
              zeroFrame <- as.data.frame(matrix(ncol=length(avp),nrow=0,dimnames=list(c(),avp)))
              for (ip in 1:nop){
                particleSets[[stockKey(ip)]] <- zeroFrame
                particleSets[[leaveKey(ip)]] <- zeroFrame
              }
              outKey="particles_leaving_the_system"
               
              # enforce initial conditions
              for (ip in 1:nop){
                particleSets[[stockKey(ip)]] <- rbind(
                  particleSets[[stockKey(ip)]],
                  enteringSet(st,iv[[ip]],ip)
                )
              }
              # initialize
            	results=as.data.frame(matrix(ncol=length(tasklist)+1,nrow=0,dimnames=list(c(),c("time",names(tasklist)))))
            	t_old=st
            	for (it in 2:nt){
                t=times[[it]]
                sts=t-t_old				#timestepsize	

                particleSets[[outKey]] <- zeroFrame
                for ( i in 1:nop){

                  # compute the number of particles 
                  # entering the system in this timestep
                  ii=function(t){return(vI(t)[i,])}
                  np_new=integrate(ii,lower=t,upper=t+sts)$val
            		  particleSets[[stockKey(i)]] <- rbind(
                    particleSets[[stockKey(i)]],
                    enteringSet(t,np_new,i)
                  ) 
                  
            		  #np=nrow(particleSets[[stockKey(1)]])
                  o_i <- Os[[i]]
                  c_i <-Cs[[i]] # c_i is a function of time (spline)
                  
                  # compute the probability for a single particle 
                  # to leave pool i during the present timestep 
                  # this will usually depend on the solution
                  delta_oi <-integrate(o_i,lower=t,upper=t+sts)$val

            		  p_leave <- delta_oi/c_i(t+sts) 
            		  
                  l=extract(particleSets[[stockKey(i)]],p_leave)
            		  particleSets[[leaveKey(i)]] <- l[["Destination"]]
                  
                  # update dataframe of particles still in the pool
            		  particleSets[[stockKey(i)]] <- l[["UpdatedSource"]]
                  
                  
                  # distribute the particles leaving pool i to the receiving pools 
                  # specified by the model
                  # a) find the receiving pools for output from pool i
                  recs=getOutputReceivers(op,i)
                  for (j in recs){
                    # compute the probability for a particle to be injected into 
                    # pool j under the assumption of being emitted by pool i
                    t_ij <- Tr[[key(i,j)]]
                    o_ij <- function(time){
                      return(o_i(time)*t_ij(time))
                    } 
                   
                    p_inject <- integrate(o_ij,lower=t,upper=t+sts)$val/delta_oi
                    targetKey=stockKey(j)
                    Source <- particleSets[[leaveKey(i)]]
                    Destination <- particleSets[[targetKey]]
                    l=transmit(
                      Source=particleSets[[leaveKey(i)]],
                      Destination=particleSets[[targetKey]],
                      p_inject)
            		    particleSets[[targetKey]] <- l[["Destination"]]
                    
                    # update dataframe of particles still in the pool
            		    particleSets[[leaveKey(i)]] <- l[["UpdatedSource"]]
                  }
                  # after all the receiving pools had there share the 
                  # rest of the output is emitted

                  particleSets[[outKey]] <-  rbind(
                    particleSets[[outKey]],
                    particleSets[[leaveKey(i)]]
                  )
                }
                if (nrow(particleSets[[outKey]]) >0){
                  particleSets[[outKey]][,"t_exitSystem"]=t
                }
            	  
                # compute the results specified in the tasklist

                resultlist=list()
                for (name in names(tasklist)){
                  resultlist[[name]] <- eval(tasklist[[name]])
                }
            		results=rbind(results,resultline(t,resultlist))
                t_old <- t
            	}	
            	return(results)
            }
      simParams=list(t)
            #r1=singleThreadParticleSimulator(simParams)
            np=detectCores()
            #np=1
            dfl=mclapply(rep(simParams,np),singleThreadParticleSimulator,mc.cores=np)
            cr=reduce2singledf(dfl)
            print(cr)
      return(list(tcn=tcn,cr=cr))
  }
)




