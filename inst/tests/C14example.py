from Rexample import *
class C14example(Rexample):
    def __init__(self,name,matrix,iv,inputrates,c14fraction):
        super(C14example,self).__init__(name,matrix,iv,inputrates)
        self.c14fraction=c14fraction
        self.addanls14()
        self.trunk="runit.automatic.c14."+name
####################################################################################
    def addanls14(self):
        m,inputrates,c_sym_strs,n,f= self.matrix,self.inputrates,self.c_sym_strs,self.n,self.c14fraction
        th=5730
        k=log(Rational(1,2))/th
        I=eye(n)
        mn=m+k*I
        t= Symbol("t")
        tau= Symbol("tau")
        self.anls14=(mn*t).exp()*self.c_sym*f+integrate((mn*tau).exp()*inputrates*f,(tau,0,t))

    def setUpVars(self):
        pp=super(C14example,self)
        Text=pp.setUpVars()
        Text+="\
   Fc=new(\"TimeMap\",t_start,t_end,function(t){.500})\n\
   th=5730\n\
   k=log(0.5)/th\n"
        return(Text)
####################################################################################
    def sols(self):
        pp=super(C14example,self)
        Text=pp.sols()
        Text+="\
   Y14=matrix(ncol="+str(self.n)+",nrow=length(t))\n"

        for j in range(self.n):
           Text+=(self.shift+"Y14[,"+str(j+1)+"]="+str(self.anls14[j])+"\n")

        return(Text)
####################################################################################
    def setUpModel(self):
        Text="\
   mod=GeneralModel_14(\n\
    t,\n\
    A,\n"\
        +rlistprint(self.c_sym_strs,self.shift)\
        +",\n"+self.shift+"inputrates,\n"\
        +self.shift+"Fc,\n"\
        +self.shift+"k,\n"\
        +self.shift+"deSolve.lsoda.wrapper\n   )\n\
   Y14ode=getC14(mod) \n\
   Yode=getC(mod) \n\
   Rode=getReleaseFlux(mod) \n"
        return(Text)
####################################################################################
    def plots(self):
        n=self.n
        pp=super(C14example,self)
        Text=pp.plots()
        Text+="\
   plot(t,Y14[,1],type=\"l\",lty=lt1,col=1,ylab=\"14C-Concentrations\",xlab=\"Time\",ylim=c(min(Y14),max(Y14)))\n"

        Text+=(self.shift+"lines(t,Y14ode[,1],type=\"l\",lty=lt2,col=1)\n")
        collist="c(1,1"
        for j in range(2,n+1):
           colstr=str(j)
           Text+=(self.shift+"lines(t,Y14[,"+str(j)+"],type=\"l\",lty=lt1,col="+colstr+")\n")
           collist+=","+colstr
           Text+=(self.shift+"lines(t,Y14ode[,"+str(j)+"],type=\"l\",lty=lt2,col="+colstr+")\n")
           collist+=","+colstr
        collist+=")"
        return(Text)
####################################################################################
    def checks(self):
        n=self.n
        pp=super(C14example,self)
        Text=pp.plots()
        Text="\
# begin checks \n\
   tol=.02*max(Y14)/tn\n\
   checkEquals(\n\
    Y14,\n\
    Y14ode,\n\
    \"test numeric solution for 14C-Content computed by the ode mehtod against analytical\",\n\
    tolerance = tol,\n\
   )\n"
        return(Text)

