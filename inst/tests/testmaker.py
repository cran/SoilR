#!/usr/bin/python
from sympy.interactive.printing import init_printing
init_printing(use_unicode=True, wrap_line=False, no_global=True)
from sympy.matrices import *
from sympy.matrices.matrices import exp_block
from sympy.matrices.matrices import jblock_exponential
from sympy import Symbol,exp,factorial,log
from sympy import latex
from sympy import integrate
import re
from sympy import simplify
from sympy import Basic, Symbol, Integer, C, S, Dummy, Rational, Add, Pow
import inspect
import difflib
class Rexample(object):
    def __init__(self,name,matrix,iv,inputrates):
        self.name=name
        self.matrix=matrix
        self.iv=iv
        self.inputrates=inputrates

    def write2file(self):
        name=self.name
        inputrates=self.inputrates
        m=self.matrix
        ck=self.iv
        n=m.rows
        c_sym=zeros(n,1)
        c_sym_strs=[]
        t= Symbol("t")
        tau= Symbol("tau")
        #t0= Symbol("t0")
        symbolprefix="c0"
        for i in range(n):
            s=symbolprefix+str(i+1)
            c_sym_strs.append(s)
            c_sym[i,0]=Symbol(s)
        anls=(m*t).exp()*c_sym+integrate((m*tau).exp()*inputrates,(tau,0,t))
        testvec=ones(1,n)
        respcoeffs=-testvec*m
        print("respcoeff=\n",respcoeffs)
        anlresp=(respcoeffs.transpose()).multiply_elementwise(anls)
        shift="   "
        Text="\
# This test function is automatically produced by the python script:"+inspect.getfile(inspect.currentframe())+"\n\
test."+name+"=function(){\n\
   require(RUnit)\n\
   require(SoilR)\n\
   t_start=0\n\
   t_end=2\n\
   tn=100\n\
   tol=.02/tn\n\
   print(tol)\n\
   timestep=(t_end-t_start)/tn\n\
   t=seq(t_start,t_end,timestep)\n\
   A=TimeMap.new(t_start,t_end,function(t){"+rmatrixprint(m,shift)+"})\n"
        
        for j in range(n):       
           Text+=(shift+c_sym_strs[j]+"="+str(ck[j])+"\n")
        Text+="\
   inputrates=TimeMap.new(t_start,t_end,function(t){return("+rmatrixprint(inputrates,shift)+")})\n\
   Y=matrix(ncol="+str(n)+",nrow=length(t))\n"
        
        for j in range(n):       
           Text+=(shift+"Y[,"+str(j+1)+"]="+str(anls[j])+"\n")
           
        Text+="\
   R=matrix(ncol="+str(n)+",nrow=length(t))\n"
        
        for j in range(n):       
           Text+=(shift+"R[,"+str(j+1)+"]="+str(anlresp[j])+"\n")
        Text+="\
   mod=GeneralModel(\n\
    t,\n\
    A,\n"\
        +rlistprint(c_sym_strs,shift)\
        +",\n"+shift+"inputrates,\n"\
        +shift+"deSolve.lsoda.wrapper\n   )\n\
   Yode=getC(mod) \n\
   Rode=getReleaseFlux(mod) \n\
   checkEquals(\n\
    Y,\n\
    Yode,\n\
    \"test numeric solution for C-Content computed by the ode mehtod against analytical\",\n\
    tolerance = tol,\n\
   )\n\
   checkEquals(\n\
    R,\n\
    Rode,\n\
    \"test numeric solution for Respiration computed by the ode mehtod against analytical\",\n\
    tolerance = tol,\n\
   )\n\
   lt1=2\n\
   lt2=4\n\
   plot(t,Y[,1],type=\"l\",lty=lt1,col=1,ylab=\"Concentrations\",xlab=\"Time\")\n"
        
        Text+=(shift+"lines(t,Yode[,1],type=\"l\",lty=lt2,col=1)\n")
        collist="c(1,1"
        for j in range(2,n+1):       
           colstr=str(j)
           Text+=(shift+"lines(t,Y[,"+str(j)+"],type=\"l\",lty=lt1,col="+colstr+")\n")
           collist+=","+colstr
           Text+=(shift+"lines(t,Yode[,"+str(j)+"],type=\"l\",lty=lt2,col="+colstr+")\n")
           collist+=","+colstr
        collist+=")"   
        
           
        Text+="\
   legend(\n\
   \"topright\",\n\
     c(\n"
        
        for j in range(1,n):       
           Text+=(shift+"  \"anlytic sol for pool "+str(j)+"\",\n")
           Text+=(shift+"  \"numeric sol for pool "+str(j)+"\",\n")
              
        Text+=(shift+"  \"anylytic sol for pool "+str(n)+"\",\n")
        Text+=(shift+"  \"numeric sol for pool "+str(n)+"\"\n")
        Text+="     ),\n\
     lty=c(lt1,lt2),\n\
     col="+collist+"\n\
   )\n\
   plot(t,R[,1],type=\"l\",lty=lt1,col=1,ylab=\"Respirationfluxes\",xlab=\"Time\")\n"
        
        Text+=(shift+"lines(t,Rode[,1],type=\"l\",lty=lt2,col=1)\n")
        collist="c(1,1"
        for j in range(2,n+1):       
           colstr=str(j)
           Text+=(shift+"lines(t,R[,"+str(j)+"],type=\"l\",lty=lt1,col="+colstr+")\n")
           collist+=","+colstr
           Text+=(shift+"lines(t,Rode[,"+str(j)+"],type=\"l\",lty=lt2,col="+colstr+")\n")
           collist+=","+colstr
        collist+=")"   
        
           
        Text+="\
   legend(\n\
   \"topright\",\n\
     c(\n"
        
        for j in range(1,n):       
           Text+=(shift+"  \"anlytic sol for pool "+str(j)+"\",\n")
           Text+=(shift+"  \"numeric sol for pool "+str(j)+"\",\n")
              
        Text+=(shift+"  \"anylytic sol for pool "+str(n)+"\",\n")
        Text+=(shift+"  \"numeric sol for pool "+str(n)+"\"\n")
        Text+="     ),\n\
     lty=c(lt1,lt2),\n\
     col="+collist+"\n\
   )\n\
}"
        testFileName="runit.test.automatic."+name+".R"
        f=open(testFileName,"w")
        f.write(Text)
        f.close()

class C14example(Rexample):
    def __init__(self,name,matrix,iv,inputrates,c14fraction):
        super(C14example,self).__init__(name,matrix,iv,inputrates)
        self.c14fraction=c14fraction

    def write2file(self):
        name=self.name
        inputrates=self.inputrates
        m=self.matrix
        ck=self.iv
        f=self.c14fraction
        #compute the decayconstant from halflife 
        #and add it to the matrix
        th=5730
        k=log(0.5)/th
        n=m.rows
        I=eye(n)
        m=m+I
        c_sym=zeros(n,1)
        c_sym_strs=[]
        t= Symbol("t")
        tau= Symbol("tau")
        #t0= Symbol("t0")
        symbolprefix="c0"
        for i in range(n):
            s=symbolprefix+str(i+1)
            c_sym_strs.append(s)
            c_sym[i,0]=Symbol(s)
        # as long as the fraction of c14 f is constant we can compute an
        # analytical solution (otherwise the matrix exponential approach becomes invalid
        anls=(m*t).exp()*c_sym+integrate((m*tau).exp()*inputrates*f,(tau,0,t))
        testvec=ones(1,n)
        respcoeffs=-testvec*m
        print("respcoeff=\n",respcoeffs)
        anlresp=(respcoeffs.transpose()).multiply_elementwise(anls)
        shift="   "
        Text="\
# This test function is automatically produced by the python script:"+inspect.getfile(inspect.currentframe())+"\n\
test."+name+"=function(){\n\
   require(RUnit)\n\
   require(SoilR)\n\
   t_start=0\n\
   t_end=5000\n\
   tn=100\n\
   timestep=(t_end-t_start)/tn\n\
   t=seq(t_start,t_end,timestep)\n\
   tol=.02/tn\n\
   print(tol)\n\
   #begin analytical solutions\n\
   c01=0.5\n\
   th=5730\n\
   k=log(0.5)/th\n\
   Y=matrix(ncol="+str(n)+",nrow=length(t))\n"
        
        for j in range(n):       
           Text+=(shift+"Y[,"+str(j+1)+"]="+str(anls[j])+"\n")
           
        Text+="\
   Y=matrix(ncol=1,nrow=length(t))\n\
   Y[,1]=c01*exp(k*t)\n\
   tol=.02*max(Y)/tn\n\
   print(tol)\n\
   R=matrix(ncol=1,nrow=length(t))\n\
   R[,1]=0\n\
   #end analytical solutions\n\
   A=TimeMap.new(t_start,t_end,function(t){"+rmatrixprint(m,shift)+"})\n"
        
        for j in range(n):       
           Text+=(shift+c_sym_strs[j]+"="+str(ck[j])+"\n")
        Text+="\
   inputrates=TimeMap.new(t_start,t_end,function(t){return("+rmatrixprint(inputrates,shift)+")})\n\
   Fc=TimeMap.new(t_start,t_end,function(t){500})\n\
   mod=GeneralModel(\n\
    t,\n\
    A,\n"\
        +rlistprint(c_sym_strs,shift)\
        +",\n"+shift+"inputrates,\n"\
        +shift+"deSolve.lsoda.wrapper\n   )\n\
   Yode=getC(mod) \n\
   Rode=getReleaseFlux(mod) \n\
   checkEquals(\n\
    Y,\n\
    Yode,\n\
    \"test numeric solution for C-Content computed by the ode mehtod against analytical\",\n\
    tolerance = tol,\n\
   )\n\
   checkEquals(\n\
    R,\n\
    Rode,\n\
    \"test numeric solution for Respiration computed by the ode mehtod against analytical\",\n\
    tolerance = tol,\n\
   )\n\
   lt1=2\n\
   lt2=4\n\
   plot(t,Y[,1],type=\"l\",lty=lt1,col=1,ylab=\"Concentrations\",xlab=\"Time\")\n"
        
        Text+=(shift+"lines(t,Yode[,1],type=\"l\",lty=lt2,col=1)\n")
        collist="c(1,1"
        for j in range(2,n+1):       
           colstr=str(j)
           Text+=(shift+"lines(t,Y[,"+str(j)+"],type=\"l\",lty=lt1,col="+colstr+")\n")
           collist+=","+colstr
           Text+=(shift+"lines(t,Yode[,"+str(j)+"],type=\"l\",lty=lt2,col="+colstr+")\n")
           collist+=","+colstr
        collist+=")"   
        
           
        Text+="\
   legend(\n\
   \"topright\",\n\
     c(\n"
        
        for j in range(1,n):       
           Text+=(shift+"  \"anlytic sol for pool "+str(j)+"\",\n")
           Text+=(shift+"  \"numeric sol for pool "+str(j)+"\",\n")
              
        Text+=(shift+"  \"anylytic sol for pool "+str(n)+"\",\n")
        Text+=(shift+"  \"numeric sol for pool "+str(n)+"\"\n")
        Text+="     ),\n\
     lty=c(lt1,lt2),\n\
     col="+collist+"\n\
   )\n}"
        testFileName="runit.test.automatic."+name+".R"
        f=open(testFileName,"w")
        f.write(Text)
        f.close()

def rmatrixprint(m,shift):
    r=m.rows
    c=m.cols
    sh1=shift+"  "
    sh2=sh1+"   "
    s="matrix(\n"\
    +sh1+"nrow="+str(r)+",\n"\
    +sh1+"ncol="+str(c)+",\n"\
    +sh1+"c(\n"
    for j in range(c): #note that the ordering of the indices is different in R which fills columns first
        s+=sh2
        for i in range(r):
            suff=",  "
            s+=(str(m[i,j])+suff)
        s+="\n"
    s=s[:-(len(suff)+1)]#remove last ",  \n"
    s+="\n"+sh1+")\n"+shift+")"
    return s
def rlistprint(v,shift):
    l=len(v)
    sh1=shift+" "
    sh2=sh1+"   "
    s=sh1+"c(\n"
    suff=",\n"
    for j in range(l-1): 
        s+=(sh2+str(v[j])+suff)
    s+=(sh2+str(v[l-1])+"\n"+sh1+")")    
    return s
##########################main part with example matrices ##########################
TwoPoolConstantInputRate=Matrix(2,1,[0.1,0.2])
TwoPoolZeroInputRate=Matrix(2,1,[0,0])
OnePoolZeroInputRate=Matrix(1,1,[0])
ThreePoolConstantInputRate=Matrix(3,1,[1,2,3])
FourPoolConstantInputRate=Matrix(4,1,[1,2,3,4])
Rexample(\
        "TwopParallel_ZeroDecayInputOnly",
        Matrix(2,2,
            [
                0, 0, 
                0, 0
            ]
        ),
        [3,2],TwoPoolConstantInputRate
    ).write2file()
#C14example(\
#        "OnePool_C14_ZeroDecay_Zero",
#        Matrix(1,1,
#            [
#                0 
#            ]
#        ),
#        [1],
#        Matrix(1,1,[0]),
#        Matrix(1,1,[1.0/2])
#    ).write2file()
Rexample(\
        "TwopParallel_constantInput",
        Matrix(2,2,
            [
                -Rational(1,20),             0, 
                             0,-Rational(1,30)
            ]
        ),
        [3,2],TwoPoolConstantInputRate
    ).write2file()
Rexample(\
        "ThreepFeedback_1",
        Matrix(3,3,
            [
                -Rational(1,2), Rational(1,3),   0,
                 Rational(1,2),-Rational(2,3),   0,  
                             0, Rational(1,3),  -1   
            ]
        ),
        [3,2,2.5],
        ThreePoolConstantInputRate
    ).write2file()
Rexample(\
        "ThreepSerial_1",
        Matrix(3,3,
            [
                -Rational(1,2),             0,   0,
                 Rational(1,2),-Rational(1,3),   0,  
                             0, Rational(1,6),  -1   
            ]
        ),
        [3,2,1],
        ThreePoolConstantInputRate
    ).write2file()
Rexample(\
        "ThreepSerial_2",
        Matrix(3,3,
            [
                -1, 0, 0,
                 1,-2, 0,  
                 0, 1,-2   
            ]
        ),
        [3,2,1],
        ThreePoolConstantInputRate
    ).write2file()
Rexample(\
        "FourpSerial_1",
        Matrix(4,4,
            [
               -1, 0, 0, 0,
                1,-2, 0, 0,  
                0, 1,-2, 1,   
                0, 1, 1,-1   
            ]
        ),
        [3,2,1,0],
        FourPoolConstantInputRate
    ).write2file()
