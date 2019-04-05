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
```
#every time you want to use the package use library(), or click on the
square in the 'packages' list
library(rAviExp)#or require(rAviExp) if loaded within functions
#Good to go
#Now let's do our first model - Alpha (Angla of attack) analysis
#For simplicity let us use the default list first
Alpha_lin.default#it is a classified list
0.02"
#You could define your ones by using myalpha=list(...) then
class(myalpha)='Alpha_Lin'
a<-create(Alpha_lin.default)#create an analysis from these assumptions
#And name it a
#Now let us see what's inside
a


#The matrix (data) is folded for convenience
#to see the full frame , use
a$Out


#Now let us do some numerical Optimization
Optim(a)
#See that is fetching some interesting points
#Where are they? can we visually see them?
lines(a)#Of course, use lines() for the S3 data to do default 2d plot
#And also
plot(a)#a 3D one
#Now what if I want other values?
fetchAlpha_Clmax(a$Out)#at Cl max
fetchAlpha_Cl(a$Out,.6)#at Cl=6
#Pretty cool eh?
#-----------------------------------------------------------------
#Let us do a second analysis, still using a default a data frame
# And it follows very similar structure.
# At this stage do not worry about what the model does
(cons<-create(Constraint.default))#use parenthesis to print while
executing
#Now repeat
plot(cons)#does not look good, shift the y of legends
plot(cons,y=8)
Optim(cons)
#This solves for the feasible range of T/W~W/S and aim for the most
#energy saving condition
#You do not have to use this point as the value to take in the next
#step but this suggests the point that the kinetic constraints
#are least critical.
#Hold on, what are the constraints?
cons$Raw

#Isnt that familiar?
#Did we see $Raw before?
a$Raw#Yes, as the assumptions are a part of the study
#And the size is relatively small, we store that as well
#Now you have seen 2 models already
#They can have some connections.
#What if we suggest that data a has the lift/drag property of the
#concept, where as we use the maximum lift coefficient for take off
#to lower the speed, and then use the highest lift to drag ratio (k)
#for cruising?
takein#see this strange function here
newCons<-Constraint.default %takein% a
newCons$TO_DISTANCE$ClTO# Now it fills the new parameter in
#And we are actually connecting the different dimentions
#now look at this line:
Optim(create(Constraint.default %takein% create(Alpha_lin.default)))
#This line actually does all things above
#Although not encouraged for building fine model,
#such grammar could be fast and convenient for a coarse model
```
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
