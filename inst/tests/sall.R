#!/usr/bin/Rscript
source("prolog.R")
alltests <- defineTestSuite(
   name="allTests",
   #dirs = file.path(.path.package(package="SoilR"),
   #"tests"),
   dirs=".",
   testFileRegexp = "^runit.+\\.[rR]",
   testFuncRegexp = "^test.+",
   rngKind = "Marsaglia-Multicarry",
   rngNormalKind = "Kinderman-Ramage"
)

testResult <- runTestSuite(alltests)
printTextProtocol(testResult)
#produce exitstatus ne 0 for buildbot to notice
ef=getErrors(testResult)
n=ef$nErr+ef$nFail
if (n>0) {stop(1)}
