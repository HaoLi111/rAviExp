#FULL WORKFLOW ANIMATION
#
#
#
#
#
#
#
#
#Sys.sleep(10)

##STOP = function() Sys.sleep(.5)
#LOAD PACKAGE
library(rAviExp)

##
#PART1 ALPHA

(a = create(Alpha_lin.default))# create.class()
a$Out#FULL DATA
plot(a)#default plot.class()
#
lines(a)#default 2d lines.class()
##
Optim(a)#default numerical Optim.class()

#
#PART2 CONS
#----------------------------------------

#1 .constraint analysis
#Constraint.default

climb=list(v_v=5,
           v=10,
           H=120)
climb=CompleteCA(climb)
cruise=list(H=400,
            v=20)
cruise=CompleteCA(cruise)
turn=list(v=15,
          H=120,
          n=1-cosd(atand(15^2/9.8/25)))
turn=CompleteCA(turn)
takeoff=list(SG=10,
             H=0,
             v=12,
             mew=.4)
takeoff=CompleteCA(takeoff)

cons_init<-list(W_S=seq(from=1,to=5,by=.1),
                CLIMB=climb,
                CRUISE_V=cruise,
                TO_DISTANCE=takeoff,
                TURN=turn)
class(cons_init)='Constraint'
#Prepare previous assumptions
#1 use the linear property of airfoil
cons_init=takein(cons_init,create(Alpha_lin.default))

#
cons_init<-create(cons_init,func=c('CA_CLIMB',
                                   'CA_CRUISE_V',
                                   'CA_TO_DISTANCE',
                                   'CA_TURN'))
#results
plot(cons_init)
Optim(cons_init)

#
#----------------------------------------
#PART3 SENSITIVITY OF CONSTRAINTS

#cons_init<-readRDS('cons_init.rds')

#take off run:


library(doParallel)
registerDoParallel(cores = 6)


Sensi_TO_SG<-function(cons,SG=1:20,func= c('CA_TO_DISTANCE',
                                           'CA_CRUISE_V',
                                           'CA_TURN',
                                           'CA_CLIMB')){
  #system.time({
  crit<-foreach(i=SG,.combine = rbind) %dopar% {
    require(rAviExp)
    cons$TO_DISTANCE$SG=i
    Optim(create(cons,func =func))
  }
  #})
  crit<-cbind(SG,crit)
  crit
}

cons_init_sg<-Sensi_TO_SG(cons_init$Raw,SG=seq(from=1,to=20,by=.1))
cons_init_sg


#

plot(cons_init_sg[,1],cons_init_sg[,2],type='l',xlab='SG from initial CA',ylab='T/W')
#
plot(cons_init_sg[,1],cons_init_sg[,2],type='l',xlab='SG from initial CA',ylab='T/W',
     xlim = c(1,2))
#cruise v

Sensi_CRUISE_V_v<-function(cons,v=1:40,func= c('CA_TO_DISTANCE',
                                               'CA_CRUISE_V',
                                               'CA_TURN',
                                               'CA_CLIMB')){
  #system.time({
  crit<-foreach(i=v,.combine = rbind) %dopar% {
    require(rAviExp)
    cons$CRUISE_V$v=i
    cons$CRUISE_V<-CompleteCA(cons$CRUISE_V)
    Optim(create(cons,func =func))
  }
  #})
  crit<-cbind(v,crit)
  crit
}

system.time(cons_init_v<-Sensi_CRUISE_V_v(cons_init$Raw,v=1:40))
cons_init_v
#
plot(cons_init_v[,1],cons_init_v[,2],type='l',xlab='Cruise v for initial CA',
     ylab='T/W required')
#
plot(cons_init_v[,1],cons_init_v[,2],type='l',xlab='Cruise v for initial CA',
     ylab='T/W required',xlim=c(15,20),ylim=c(0.6,1))



#trail 1
cons_t1<-cons_init$Raw
cons_t1$TO_DISTANCE$SG=1.6
cons_t1$CRUISE_V$v=18
cons_t1$CRUISE_V<-CompleteCA(cons_t1$CRUISE_V)
cons_t1$W_S<-seq(from=1,to=6,by=.01)
cons_t1=create(cons_t1,func= c('CA_TO_DISTANCE',
                               'CA_CRUISE_V',
                               'CA_TURN',
                               'CA_CLIMB'))

#

plot(cons_t1)
Optim(cons_t1)

#saveRDS(cons_t1,'cons_t1.rds')

#-----------------------------
#PART4 COARSE WEIGHT ANALYSIS

#Weight Analysis
payload_init=list(m=.25,mFrac=.25,
                  P_draw=10)
class(payload_init)='Payload'

weight_coarse_init=list(range=1e4,
                        PAYLOAD=payload_init)#m
class(weight_coarse_init)='Weight_coarse'

weight_coarse_init<-(weight_coarse_init %takein% cons_t1)

weight_init<-create(weight_coarse_init)
weight_init

#
#-----------------------------
#PART5 SIZING

#SIZE BATTERY

Batt<-list(EnergyDensity=135,
           S=3,
           P=1,
           V_cell=3.8,
           C=25,
           Usage=.5)

#


size_Battery_coarse<-function(E,
                              ita,
                              V,
                              EnergyDensity=486000#[W*s/kg]
){
  Ee<-E/ita
  Capacity<-Ee/V
  m<-Ee/EnergyDensity

  re<-list(E=E,Ee=Ee,V=V,ita=ita,EnergyDensity=EnergyDensity,Capacity=Capacity,
           m=m)
  class(re)='Battery'
  re
  #q=Da
  #C=q*V
  #E = V*I*t
}

batt_init<-size_Battery_coarse(E = weight_init$Energy,
                               ita=.4,
                               V=3.7*3)
#
#SIZE PLANE

plane_init<-size_concept_simple(S = as.numeric(weight_init$m_gross)/(as.numeric(Optim(cons_init))[2]),
                                l=.7,
                                xWM = .2)
fuselage_init = list(Length = 1.05,
                     r =c(.05,.075,.075,.02,.01,0),
                     x =c(0,.2,.4,.9,1,1.05)
)
class(fuselage_init) ='fuse'

#
plane_init$fuselage = fuselage_init
class(plane_init) ='conventionalConcept'
str(plane_init)
#

plotxy(plane_init)

#
#
#
# CONGRATULATIONS
# YOUR WORK IS DONE
# ENJOY THE PLANE
#
#
text('Workflow Completed',x=.5,y=.5)

