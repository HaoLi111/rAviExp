# rAviExp

## Brief

R library for Aviation Exploratory design.

It is designed to solve for parameters managing multiple mathematical models in a customizable workflow. There are basic numerical functions, wrappers and especially S3 Objective functions that recognizes the type of analysis.

Unlike CAD/CAE/CAM or flight simulators or calculation gadgets. This is intended to be functional (closure) type so the modells can be more easily called at different stages of initial design calculation. The users are encouraged to record their work process in R script so that it can be easily rewritten and modified if needed at any stage.

Different models have different initial assumptions and because it is programmable (can be called with R), they can be easily customized, at different levels. Theoretically you can customize the parameters at any stage and change the decisions by simply editing the lists but the use of %takein% parameters are encouraged.

This version is the 1st released version of AviExp project.
For manuals, tutorials, worksheets, reference books and relavant projects please visit:
https://aviexptemp.weebly.com/

I intend to make the project open-source and it follows the LGPL_V3.0

I hope this could make the designers' life easier.

## Intallation

### Quick Installation
'R is a free software environment for statistical computing and graphics.' There are a
number of platforms that provide R.
First install R from (https://www.r-project.org/), or install Rstudio with R from
(https://www.rstudio.com/products/rstudio/download/), or install Microsoft R for
visual studio from (https://visualstudio.microsoft.com/zh-hans/vs/features/rtvs/?
rr=https%3A%2F%2Fwww.google.com%2F).
Open it and install devtools and use devtools to download and install rAviExp with
these R commands.
```r
install.packages('devtools')
library('devtools')
install_github('HaoLi111/rAviExpPrerelease')
```
Also you are encouraged to install
```r
install_github('HaoLi111/MFVN')
And other packages for plotting
install.packages('rgl')
install.packages('scatterplot3d')
install.packages('doParallel')
```
Alternative way is to download a built-version to your lib-path. Alternatively if you
want to install R with the packages on an SD card or an USB drive, copy R into the
drive and copy these package and dependencies to
```
YourDrive:\Program Files\R\R-3.5.1\library\
```
to run it on other computers without installing R.

### Quickstart

Basic functionalities
This section demonstrates the very basic uses of functions of the package briefly.
Sourced from (https://aviexptemp.weebly.com/features/something-interesting-totry-
with-the-1st-released-version).
#every time you want to use the package use library(), or click on the
square in the 'packages' list
library(rAviExp)#or require(rAviExp) if loaded within functions
#Good to go
#Now let's do our first model - Alpha (Angla of attack) analysis
#For simplicity let us use the default list first
Alpha_lin.default#it is a classified list
## [1] "Alpha_lin:Cla= 0.1 alpha0= -5 CdiF= 0.0397887313856378 Cd0=
0.02"
#You could define your ones by using myalpha=list(...) then
class(myalpha)='Alpha_Lin'
a<-create(Alpha_lin.default)#create an analysis from these assumptions
#And name it a
#Now let us see what's inside
a
## [1] "Generated from $Raw"
## [1] "Alpha_lin:Cla= 0.1 alpha0= -5 CdiF= 0.0397887313856378 Cd0=
0.02"
## [1] "Out : "
## alpha Cl Cd k
## 1 -5.0 0.00 0.02000000 0.0000000
## 2 -4.9 0.01 0.02000398 0.4999005
## 3 -4.8 0.02 0.02001592 0.9992049
## 4 -4.7 0.03 0.02003581 1.4973191
## 5 -4.6 0.04 0.02006366 1.9936540
## 6 -4.5 0.05 0.02009947 2.4876276
## [1] "......"
## alpha Cl Cd k
## 146 9.5 1.45 0.1036558 13.98860
## 147 9.6 1.46 0.1048137 13.92948
## 148 9.7 1.47 0.1059795 13.87061
## 149 9.8 1.48 0.1071532 13.81200
## 150 9.9 1.49 0.1083350 13.75364
## 151 10.0 1.50 0.1095246 13.69555
#The matrix (data) is folded for convenience
#to see the full frame , use
a$Out
## alpha Cl Cd k
## 1 -5.0 0.00 0.02000000 0.0000000
## 2 -4.9 0.01 0.02000398 0.4999005
## 3 -4.8 0.02 0.02001592 0.9992049
## 4 -4.7 0.03 0.02003581 1.4973191
## 5 -4.6 0.04 0.02006366 1.9936540
## 6 -4.5 0.05 0.02009947 2.4876276
## 7 -4.4 0.06 0.02014324 2.9786669
## 8 -4.3 0.07 0.02019496 3.4662106
## 9 -4.2 0.08 0.02025465 3.9497107
## 10 -4.1 0.09 0.02032229 4.4286350
## 11 -4.0 0.10 0.02039789 4.9024685
## 12 -3.9 0.11 0.02048144 5.3707152
## 13 -3.8 0.12 0.02057296 5.8328997
## 14 -3.7 0.13 0.02067243 6.2885690
## 15 -3.6 0.14 0.02077986 6.7372930
## 16 -3.5 0.15 0.02089525 7.1786662
## 17 -3.4 0.16 0.02101859 7.6123084
## 18 -3.3 0.17 0.02114989 8.0378652
## 19 -3.2 0.18 0.02128915 8.4550092
## 20 -3.1 0.19 0.02143637 8.8634396
## 21 -3.0 0.20 0.02159155 9.2628833
## 22 -2.9 0.21 0.02175468 9.6530940
## 23 -2.8 0.22 0.02192577 10.0338530
## 24 -2.7 0.23 0.02210482 10.4049687
## 25 -2.6 0.24 0.02229183 10.7662758
## 26 -2.5 0.25 0.02248680 11.1176356
## 27 -2.4 0.26 0.02268972 11.4589347
## 28 -2.3 0.27 0.02290060 11.7900849
## 29 -2.2 0.28 0.02311944 12.1110218
## 30 -2.1 0.29 0.02334623 12.4217045
## 31 -2.0 0.30 0.02358099 12.7221144
## 32 -1.9 0.31 0.02382370 13.0122541
## 33 -1.8 0.32 0.02407437 13.2921465

#Now let us do some numerical Optimization
Optim(a)
## $kmax
## alpha Cl Cd k
## 72 2.1 0.71 0.0400575 17.72452
##
## $Cdmin
## alpha Cl Cd k
## 1 -5 0 0.02 0
##
## $Clmax
## alpha Cl Cd k
## 151 10 1.5 0.1095246 13.69555
#See that is fetching some interesting points
#Where are they? can we visually see them?
lines(a)#Of course, use lines() for the S3 data to do default 2d plot
#And also
plot(a)#a 3D one
## Loading required package: scatterplot3d
#Now what if I want other values?
fetchAlpha_Clmax(a$Out)#at Cl max
## alpha Cl Cd k
## 151 10 1.5 0.1095246 13.69555
fetchAlpha_Cl(a$Out,.6)#at Cl=6
## Loading required package: MFVN
##
## Attaching package: 'MFVN'
## The following object is masked from 'package:base':
##
## Map
## alpha Cl Cd k
## 61 1 0.6 0.03432394 17.48051
#Pretty cool eh?
#-----------------------------------------------------------------
#Let us do a second analysis, still using a default a data frame
# And it follows very similar structure.
# At this stage do not worry about what the model does
(cons<-create(Constraint.default))#use parenthesis to print while
executing
## NULL
## W_S CA_TURN CA_ENERGY_LEVEL CA_CLIMB CA_CRUISE_V
## [1,] 5.0 0.9614092 1.0113307 0.8755383 0.9621314
## [2,] 5.1 0.9425622 0.9924821 0.8649651 0.9432989
## [3,] 5.2 0.9244402 0.9743585 0.8547997 0.9251913
## [4,] 5.3 0.9070022 0.9569189 0.8450190 0.9077677
## [5,] 5.4 0.8902100 0.9401252 0.8356017 0.8909900
## [6,] 5.5 0.8740286 0.9239421 0.8265279 0.8748229
## CA_SERVICE_CEILING CA_TO_DISTANCE
## [1,] 7.673934 7.249945
## [2,] 7.598968 7.125045
## [3,] 7.526176 7.004949
## [4,] 7.455454 6.889385
## [5,] 7.386705 6.778102
## [6,] 7.319840 6.670865
## [1] "......"
## W_S CA_TURN CA_ENERGY_LEVEL CA_CLIMB CA_CRUISE_V
## [446,] 49.5 0.09814710 0.1473694 0.4025223 0.1052967
## [447,] 49.6 0.09795344 0.1471742 0.4024416 0.1051175
## [448,] 49.7 0.09776057 0.1469797 0.4023613 0.1049390
## [449,] 49.8 0.09756849 0.1467861 0.4022815 0.1047614
## [450,] 49.9 0.09737718 0.1465932 0.4022022 0.1045845
## [451,] 50.0 0.09718665 0.1464011 0.4021232 0.1044084
## CA_SERVICE_CEILING CA_TO_DISTANCE
## [446,] 2.483378 1.523486
## [447,] 2.480939 1.522189
## [448,] 2.478508 1.520897
## [449,] 2.476083 1.519610
## [450,] 2.473666 1.518329
## [451,] 2.471257 1.517052
#Now repeat
plot(cons)#does not look good, shift the y of legends
plot(cons,y=8)
Optim(cons)
## Min W_S_optim.W_S
## 2.471257 50.000000
#This solves for the feasible range of T/W~W/S and aim for the most
#energy saving condition
#You do not have to use this point as the value to take in the next
#step but this suggests the point that the kinetic constraints
#are least critical.
#Hold on, what are the constraints?
cons$Raw
## [1] "W/S ranging:"
## [1] 5
## [1] 50
## [1] "D W/S"
## [1] 0.1
## [1] "Constant v turn"
## List of 6
## $ Cd_min: num 0.02
## $ v : num 20
## $ H : num 200
## $ k : num 0.0398
## $ n : num 0.357
## $ q : num 240
## [1] "Energy Level"
## List of 8
## $ Cd_min: num 0.02
## $ k : num 0.0398
## $ n : num 0.181
## $ Ps : num 1
## $ v : num 20
## $ H : num 200
## $ rho : num 1.2
## $ q : num 240
## [1] "Rate of Climb"
## List of 7
## $ v_v : num 5
## $ v : num 15
## $ k : num 0.0398
## $ Cd_min: num 0.02
## $ H : num 200
## $ q : num 135
## $ rho : num 1.2
## [1] "Cruise v"
## List of 7
## $ H : num 200
## $ k : num 0.0398
## $ Cd_min: num 0.02
## $ v : num 20
## $ H : num 200
## $ rho : num 1.2
## $ q : num 240
## [1] "Service_CEILING"
## List of 6
## $ v_v : num 20
## $ Cd_min: num 0.02
## $ H : num 400
## $ k : num 0.0398
## $ rho : num 1.18
## $ q : num 236
## [1] "takeoff"
## List of 8
## $ ClTO: num 1.2
## $ CdTO: num 0.5
## $ H : num 0
## $ v : num 20
## $ SG : num 30
## $ mew : num 0.2
## $ rho : num 1.22
## $ q : num 245
#Isnt that familiar?
#Did we see $Raw before?
a$Raw#Yes, as the assumptions are a part of the study
## [1] "Alpha_lin:Cla= 0.1 alpha0= -5 CdiF= 0.0397887313856378 Cd0=
0.02"
#And the size is relatively small, we store that as well
#Now you have seen 2 models already
#They can have some connections.
#What if we suggest that data a has the lift/drag property of the
#concept, where as we use the maximum lift coefficient for take off
#to lower the speed, and then use the highest lift to drag ratio (k)
#for cruising?
takein#see this strange function here
## function(x,...) UseMethod('takein')
## <bytecode: 0x000000001dcbbd90>
## <environment: namespace:rAviExp>
newCons<-Constraint.default %takein% a
newCons$TO_DISTANCE$ClTO# Now it fills the new parameter in
## [1] 1.5
#And we are actually connecting the different dimentions
#now look at this line:
Optim(create(Constraint.default %takein% create(Alpha_lin.default)))
## Min W_S_optim.W_S
## 2.471257 50.000000
#This line actually does all things above
#Although not encouraged for building fine model,
#such grammar could be fast and convenient for a coarse model
General structure of the package [needed update]
The package contains a number of models that can be applied in different stages of
design. But essentially there are 3 things to remember. 1. Create or import a study
Create a list with certain structure specified Classify the list Create the study and
print the results 2. To extract properties from results Use Optim() to find
numerically or symbolically some specific points Use plot(), lines() and %plot% to
create default plots Use Analyse() to find suggestive values Use Extract() to try to
inversely find the properties from the result (if the raw is not found). 3. Link them
with the data structure Use %takein% or %>takein>% to assign parameters to new
study Load and save data These functions will be further demonstrated in the case
studies. For a more formal introduction of each function, use help() to find the
documentation. E.g.
help(create)
Underlying Physics ideas about Aircraft design:Introduction to
Relevant Physics Ideas of Airplane Conceptual Design (2nd Ed.)
by Hao Li [need update]
Rewrite from
[Introduction_to_Relevant_Physics_Ideas_in_Airplane_Conceptual_Design.pdf] This
was rewritten from my physics internal paper in March 2018.
Introduction to Relevant Physics Ideas of Airplane Conceptual Design
There are many physis ideas linked to aircraft conceptual design. For people doing
evaluations and design works it is cruicial to have a good understanding of not only
the physics theories but also the connections between them.


# Demo

A demo ca be found there

https://github.com/HaoLi111/rAviExp/blob/master/demo/demo1missionspcfc.R
