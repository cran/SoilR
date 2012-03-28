#!/usr/bin/python
from C14example import *
from Manzoniexample import *
#########################################################################
#########################################################################
#########################################################################
##########################main part with example matrices ##########################
TwoPoolConstantInputRate=Matrix(2,1,[0.1,0.2])
TwoPoolZeroInputRate=Matrix(2,1,[0,0])
OnePoolZeroInputRate=Matrix(1,1,[0])
ThreePoolConstantInputRate=Matrix(3,1,[1,2,3])
FourPoolConstantInputRate=Matrix(4,1,[1,2,3,4])
Rexample(\
        "TwopParallel_ZeroInput",
        Matrix(2,2,
            [
                -Rational(1,20),             0, 
                             0,-Rational(1,30)
            ]
        ),
        [3,2],TwoPoolZeroInputRate
    ).write2file()
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
C14example(\
        "OnePool_C14_ZeroDecay_Zero",
        Matrix(1,1,
            [
                0           #Decay matrix
            ]
        ),
        [1],                #c0 =initial c value >>>not<<< c14
        Matrix(1,1,[0]),    #Inputrate
        Matrix(1,1,[1.0/2]) #c14fraction
    ).write2file()
C14example(\
        "OnePool_C14_ZeroDecay_constantInputrate",
        Matrix(1,1,
            [
                0           #Decay matrix
            ]
        ),
        [1],                #c0 =initial c value >>>not<<< c14
        Matrix(1,1,[1]),    #constant Inputrate
        Matrix(1,1,[1.0/2]) #c14fraction
    ).write2file()
th=5730
k=log(Rational(1,2))/th
C14example(\
        "OnePool_C14_equalDecay_ZeroInput",
        Matrix(1,1,
            [
                k           #Decay constant equal to the c14 radiaaktive decay
            ]
        ),
        [1],                #c0 =initial c value >>>not<<< c14
        Matrix(1,1,[0]),    #Inputrate
        Matrix(1,1,[1.0/2]) #c14fraction
    ).write2file()
C14example(\
        "TwoPool_C14_equalDecay_ZeroInput",
        Matrix(2,2,
            [
                k ,0,           #Decay constant equal to the c14 radiaaktive decay
                0 ,k
            ]
        ),
        [1,2],                #c0 =initial c value >>>not<<< c14
        Matrix(2,1,[0,0]),    #Inputrate
        1.0/2 #c14fraction
    ).write2file()

################################################################################
#we follow the manzoni nomenclature here
r=Rational(1,4) 
#r is the released fraction of the output of pool1 for r=1 all C is released
# and the second pool recieves nothing
# if r=0 all the output of the first pool is without loss transferred to the
# second pool
inputfractions=[1,0]
# this means that pool one receives all the input
k1=Rational(1,20)
k2=Rational(1,30)
#the decay constants are considered positive in the Manzoni paper
Manzoniexample(\
        "TwopSeriell",
        Matrix(2,2,
            [
                      -k1,  0, 
                 (1-r)*k1,-k2
            ]
        ),
        [1,0]
    ).write2file()
################################################################################
#we follow the manzoni nomenclature here
alpha=Rational(1,3)
# alpha is the fraction of the inputrate that goes to the first pool
k1=Rational(1,20)
k2=Rational(1,30)
#the decay constants are considered positive in the Manzoni paper
Manzoniexample(\
        "TwopParallel",
        Matrix(2,2,
            [
              -k1,  0,              
              0  ,-k2
            ]
        ),
        [alpha,1-alpha]
    ).write2file()
################################################################################
#we follow the manzoni nomenclature here
r=Rational(1,4) 
#r is the released fraction of the output of pool1 for r=1 all C is released
# and the second pool recieves nothing
# if r=0 all the output of the first pool is without loss transferred to the
# second pool
f=1 
# f is the fraction of the material leaving the second pool that is fed back to 
# the first one . In the Manzoni paper it is always set to one 
inputfractions=[1,0]
# this means that pool one receives all the input
k1=Rational(1,20) 
k2=Rational(1,30) 
Manzoniexample(\
        "TwopFeedback",
        Matrix(2,2,
            [
                     -k1, f*k2, 
                (1-r)*k1,  -k2
            ]
        ),
        [3,2]
    ).write2file()
################################################################################
