#!/usr/bin/Rscript
source("prolog.R")
library("deSolve")
#attr(GeneralModel_14,"ex")()
#Res=runTestFile("runit.test.Model.R")
load("../../data/C14Atm_NH.rda")
#runTestFile("runit.OnePool_C14_ZeroDecay_Zero.R")
runTestFile("runit.test.automatic.OnePool_C14_ZeroDecay_Zero.R")
#printTextProtocol(Res)
#ef=getErrors(Res)
#n=ef$nErr+ef$nFail
#if (n>0) {stop(1)}
