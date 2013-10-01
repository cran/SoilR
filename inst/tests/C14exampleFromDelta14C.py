from C14example import *
def delta14C2AbsoluteFractionNormal(d14):
    return(d14/1000.0+1.0)
####################################################################################
class C14exampleFromDelta14C(C14example):
    def __init__(self,name,matrix,iv,iF,inputrates,c14fraction_Delta14C):
        super(C14example,self).__init__(name,matrix,iv,inputrates)
        self.name=name+"_c14_fromDelta14C"
        self.c14fraction_Delta14C=c14fraction_Delta14C
        self.trunk="runit.automatic.c14_fromDelta14C."+name
        self.addanls14()
        self.iF=iF
####################################################################################
    def addanls14(self):
	fc_AFM=delta14C2AbsoluteFractionNormal(self.c14fraction_Delta14C)
        F0_AFM=Matrix(map(delta14C2AbsoluteFractionNormal,self.symbolicF0()))
	super(C14exampleFromDelta14C,self).addanls14_fromAbsoluteFractionModern(fc_AFM,F0_AFM)
	
####################################################################################
    def setUpVars(self):
        pp=super(C14example,self)
        Text=pp.setUpVars()
        for j in range(self.n):       
           Text+=(self.shift+self.f_sym_strs[j]+"="+str(self.iF[j])+"\n")
        Text+="\
   initialF=SoilR.F0.new("+rlistprint(self.f_sym_strs,self.shift)+",\n format=\"Delta14C\")\n\
   Fc=new(\"FcAtm\",t_start,t_end,function(t){"+str(self.c14fraction_Delta14C)+"},format=\"Delta14C\")\n\
   th=5730\n\
   k=log(0.5)/th\n"
        return(Text)
