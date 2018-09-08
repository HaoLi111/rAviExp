#Constraint Configuration
# Global Cl
Config<-function(x,...) UseMethod('Config')
Config.Constraint<-function(cons = Constraint.template,Alpha){
  Cdmin = fetchAlpha_Cdmin(Alpha)[3]

}




#
k<-function(AR = 8, e= 1) (3.141593 * AR * e)^(-1)
Q<-function(v,H = 0) .5*Rho(H)*v^2#Potential Energy Density
Rho<-function(H = 0) 101.325e3*(1-H*.0065/288.15)^(9.80665*0.0289644/8.31447/.0065)*0.0289644/8.31447/(288.15-H*0.0065)
cosd<-function(x) cos(x/180*3.141593)
#Constraint
CompleteQ<-function(List){
	List$q<-Q(List$v,List$H)
	List
}
#completeRho
CompleteRho<-function(List){
  List$rho = Rho(H = List$H)
  List
}
CompleteCA<-function(List){
    List<-CompleteRho(List)
    List<-CompleteQ(List)
    List
}


turn.template = list(Cdmin = NA,
                     v = NA,
                     H = NA,
                     k = NA,
                     n = NA,
                     q =NA)
#1
turn.default<-list(Cd_min = .02,
				   v = 20,
				   H = 200,
				   k = k(),
				   n = 1-cosd(50))
turn.default = CompleteQ(turn.default)
#2
energy_level.template = list(Cd_min = NA,
                             k = NA,
                             n = NA,
                             Ps = NA,
                             v = NA,
                             H = NA,
                             q =NA)
energy_level.default = list(Cd_min = .02,k = k(), n  = 1-cosd(35), Ps = 1, v = 20,H=200)
energy_level.default = CompleteRho(energy_level.default)
energy_level.default = CompleteQ(energy_level.default)
#3
#Created from canterbury on 2018-04-05 16:21:26.
climb.template<-list(v_v = NA,
                     v = NA,
                     k = NA,
                     Cd_min = NA,
                     H = NA,
                     q = NA)
climb.default<-list(v_v = 5,v =15,k =k(), Cd_min = .02,H = 200)
climb.default<-CompleteQ(climb.default)
climb.default<-CompleteRho(climb.default)

#4
cruise_v.template = list(H = NA,
                         v = NA,
                         q = NA)
cruise_v.default=list(H=200,k=k(), Cd_min = .02,v = 20,H=200)
cruise_v.default= CompleteRho(cruise_v.default)
cruise_v.default = CompleteQ(cruise_v.default)

#5
service_ceiling.template<-list(v_v=NA,
                             Cd_min=NA,
                             H=NA,
                             rho=NA,
                             k=NA)
service_ceiling.default<-list(v_v = 20,Cd_min = .02,H = 400,k=k())
service_ceiling.default<-CompleteCA(service_ceiling.default)


#5
takeoff.default = list(ClTO = 1.2,CdTO =.5,H=0,v=20,SG=30,mew=.2)
takeoff.default =CompleteCA(takeoff.default)
takeoff.template =list(ClTO = NA,CdTO = NA,H=NA,v=NA,SG=NA,mew=NA)
#CA_TO_DISTANCE(takeoff.default,W_S = 1)

Constraint.template<-list(W_S = NA,
				TURN = turn.template,
				ENERGY_LEVEL = energy_level.template,
				CLIMB = climb.template,
				CRUISE_V = cruise_v.template,
				SERVICE_CEILING = service_ceiling.template,
				CL_MAX_STALL = list(q=NA),
				TO_DISTANCE = takeoff.template)
class(Constraint.template)<-'Constraint'

Constraint.default<-list(W_S = seq(from = 5, to = 50,by = .1),
				TURN = turn.default,
				ENERGY_LEVEL = energy_level.default,
				CLIMB = climb.default,
				CRUISE_V = cruise_v.default,
				SERVICE_CEILING = service_ceiling.default,
				CL_MAX_STALL = list(q=50),
				TO_DISTANCE = takeoff.default)
class(Constraint.default)<-'Constraint'

print.Constraint<-function(cons = Constraint.default){
    print('W/S ranging:')
    print(min(cons$W_S))
    print(max(cons$W_S))
    print('D W/S')
    print(cons$W_S[2] - cons$W_S[1])
    print('Constant v turn')
    str(cons$TURN)
    print('Energy Level')
    str(cons$ENERGY_LEVEL)
    print('Rate of Climb')
    str(cons$CLIMB)
    print('Cruise v')
    str(cons$CRUISE_V)
    print('Service_CEILING')
    str(cons$SERVICE_CEILING)
    print('takeoff')
    str(cons$TO_DISTANCE)
}
#---------------------------------------------------------------------------
#1
CA_TURN<-function(x,...) UseMethod('CA_TURN')
CA_TURN.Constraint<-function(Cons = Constraint.default){
	CA_TURN.list(Cons$TURN,W_S = Cons$W_S)
}
CA_TURN.numeric<-function(W_S=get("W_S"),
  Cd_min = get("CA$Cd_min"),
                  q=get("q_default"),
                  k=get("k_default"),
                  n=get("n_default")
){
  T_W = q*(Cd_min/W_S+k*(n/q)^2*(W_S))
  return(T_W)
}
CA_TURN.list<-function(x,W_S){
	ve=numeric(length(W_S))
	for (i in seq_along(W_S)){
		ve[i] = CA_TURN.numeric(W_S[i],x$Cd_min,x$q,x$k,x$n)
	}
	ve
}

#CA_TURN(turn.default,1:20)
#2
CA_ENERGY_LEVEL<-function(x,...) UseMethod('CA_ENERGY_LEVEL')
CA_ENERGY_LEVEL.Constraint<-function(Cons = Constraint.default){
    CA_ENERGY_LEVEL.list(Cons$ENERGY_LEVEL,W_S = Cons$W_S)
}
CA_ENERGY_LEVEL.numeric<-function(W_S,
                          q,
                          Cd_min=.01,
                          k,
                          n,
                          Ps,
                          v){
  T_W = q* (Cd_min/(W_S) + k * (n/q)^2 * (W_S)) + Ps/v
  T_W
}
CA_ENERGY_LEVEL.list<-function(x, W_S){
	EnergyLevel=numeric(length(W_S))
	for (i in seq_along(W_S)){
		EnergyLevel[i] = CA_ENERGY_LEVEL.numeric(W_S[i],x$q,x$Cd_min,x$k,x$n,x$Ps,x$v)
	}
	EnergyLevel
}
#CA_ENERGY_LEVEL(energy_level.default,1:20)
#CA_ENERGY_LEVEL()
#3
CA_CLIMB<-function(x,...) UseMethod('CA_CLIMB')
CA_CLIMB.Constraint<-function(Cons) CA_CLIMB.list(Cons$CLIMB,Cons$W_S)
CA_CLIMB.numeric<-function(W_S,
                   v_v,
                   v,
                   k,
                   q,
                   Cd_min = .01){
  T_W = v_v/v + q/W_S*Cd_min + k/q*W_S
  T_W
}
CA_CLIMB.list<-function(x,W_S){
    Climb<-numeric(length(W_S))
    for (i in seq_along(W_S)){
        Climb[i] = CA_CLIMB.numeric(W_S[i],x$v_v,x$v,x$k,x$q,x$Cd_min)
    }
    Climb
}

#4
CA_CRUISE_V<-function(x,...) UseMethod('CA_CRUISE_V')
CA_CRUISE_V.Constraint<-function(Cons) CA_CRUISE_V.list(Cons$CRUISE_V,Cons$W_S)
CA_CRUISE_V.numeric<-function(W_S,
                      Cd_min = .01,
                      q,
                      k){
  T_W = q*Cd_min*(1/W_S) + k * (1/q) * (W_S)
  T_W
}
CA_CRUISE_V.list<-function(x,W_S){
    Cruise_v<-numeric(length(W_S))
    for(i in seq_along(W_S)){
        Cruise_v[i]<-CA_CRUISE_V.numeric(W_S[i],x$Cd_min,x$q,x$k)
    }
    Cruise_v
}
#CA_CRUISE_V(Constraint.default)
CA_SERVICE_CEILING<-function(x,...) UseMethod('CA_SERVICE_CEILING')
CA_SERVICE_CEILING.numeric<-function(W_S,
                             v_v,
                             Cd_min,
                             H,
                             rho,
                             k){
  T_W = v_v/sqrt(2/rho * W_S * sqrt(k/3/Cd_min) ) + 4 * sqrt(k*Cd_min/3)
  T_W
}
CA_SERVICE_CEILING.list<-function(x,W_S){
    Service_Ceiling<-numeric(length(W_S))
    for(i in seq_along(W_S)){
        Service_Ceiling[i]<-CA_SERVICE_CEILING.numeric(W_S[i],x$v_v,x$Cd_min,x$H,x$rho,x$k)
    }
    Service_Ceiling
}
CA_SERVICE_CEILING.Constraint<-function(Cons) CA_SERVICE_CEILING.list(Cons$SERVICE_CEILING,Cons$W_S)
#CA_SERVICE_CEILING(Constraint.default)
#CA



CA_TO_DISTANCE<-function(x,...) UseMethod('CA_TO_DISTANCE')
CA_TO_DISTANCE.numeric<-function(W_S,
                                 ClTO,
                                 CdTO,
                                 q,
                                 SG,#ground run
                                 v,
                                 mew){
  q = q/2#lift off/sqrt(2)
  v*v/2/9.803/SG+ (q*CdTO)/W_S+mew*(1-q*ClTO/W_S)
}

CA_TO_DISTANCE.list = function(x,W_S){
  ve=numeric(length(W_S))
  for (i in seq_along(W_S)){
    ve[i] = CA_TO_DISTANCE.numeric(W_S[i],x$ClTO,x$CdTO,x$q,x$SG,x$v,x$mew)
  }
  ve
}
CA_TO_DISTANCE.Constraint<-function(ca) CA_TO_DISTANCE.list(ca$TO_DISTANCE,ca$W_S)





create<-function(x,...) UseMethod('create')

create.Constraint<-function(Cons = constraint.default,func=c("CA_TURN",
                                     "CA_ENERGY_LEVEL",
                                     "CA_CLIMB",
                                     "CA_CRUISE_V",
                                     "CA_SERVICE_CEILING",
                                     'CA_TO_DISTANCE')){
	m<-matrix(rep(0,times = (length(func)+1) *length(Cons$W_S)),ncol =length(func)+1)
	m[,1]<-Cons$W_S
	for(i in seq_along(func)){
	    m[,i+1]<-get(func[i])(Cons)
	}
	colnames(m)=c('W_S',func)
	m = list(Raw = Cons,Out=m)
	class(m)<-'ConstraintOut'
	m
}

#ca<-create(Constraint.default)

#Constraint DATA Manipulations functions
#createConstraint
createConstraint<-function(W_S = get("W_S_default"),
                           constraint = get("constraint"),
                           state = c("CA_TURN",
                                     "CA_ENERGY_LEVEL",
                                     "CA_CLIMB",
                                     "CA_TO_DISTANCE",
                                     "CA_CRUISE_V",
                                     "CA_SERVICE_CEILING",
                                     "CA_CL_MAX_STALL",
                                     "CA_TAKEOFF",
                                     "CA_LANDING")){
  W_S<-DATA.frame(W_S)
  #print(constraint)
  for(func in state){
    W_S<-within(W_S,{
      assign(func,sapply(W_S,get(func)))
    })
  }
  return(W_S)
}






#plotConstrait
#Given a DATAset with the 1st column as W_S, other as the other valiables (T/F_G)
#Plot the Constraint Diagram with legend
plotConstraintOut<-function(DATA,target = "T/F_G",
                         legendlocation = "top"){
  with(DATA,{
    matplot(DATA[,-1],type = 'l',xlab = "W/S",ylab = target,lty = 1)
    legend(legendlocation,colnames(DATA),col = 2:length(ncol(DATA)),cex = 1,fill = 2:length(ncol(DATA)))
  })

}

print.ConstraintOut<-function(DATA){
  print(DATA$Constraint)
  print(head(DATA$Out))
  print('......')
  print(tail(DATA$Out))
}

plot.ConstraintOut<-function(ca,y=1.4){
  o = Optim(ca)
  #ca=ca$Out
  matplot(ca$Out[,1],ca$Out
          [,-1],xlab ='W / S',ylab = 'T / W',type = 'l',main =paste( 'Constraint Analysis of',deparse(substitute(ca)),sep=' '))
  legend(max(ca$Constraint$W_S)/2,y=max(ca$Out[,-1]),bty='n',legend=(colnames(ca$Out)[-1]),col = 2:ncol(ca$Out),lty = 2:ncol(ca$Out))

  points(o[2],o[1],col = 'red',pty = 3)
}

#optimConstraint
#find the absolute minimum (numerical) value of the target variable of the constraint analysis
#(T/F_G) in the feasible region

Optim<-function(x,...) UseMethod('Optim')
Optim.ConstraintOut<-function(DATA){
  DATA<-DATA$Out
  restrictMax<-apply(DATA[,-1],1,max)
  Min=min(restrictMax)
  W_S_optim<-DATA[which(restrictMax == Min),1]
  re=c(Min = Min, W_S_optim = W_S_optim)
  return(re)
}




CA_CL_MAX_STALL.numeric<-function(W_S,q) W_S/q



#Sensitivity 1 parameter



e_GAAD_M1<-function(AR) 1.78*(1-.045*AR^0.68)-.64 #*GAAD P64 Method 1 (9.5.14)

k_PENET<-function(AR=8) k(AR,e = e_GAAD_M1(AR))

DefCA_k_AR<-function(Cons,AR){
	Cons$TURN$k<-k_PENET(AR)
	Cons$ENERGY_LEVEL$k<-k_PENET(AR)
	Cons$CLIMB$k<-k_PENET(AR)
	Cons$CRUISE_V$k<-k_PENET(AR)
	Cons$CRUISE_V$k<-k_PENET(AR)
	Cons
}

Sensi<-function(x,...) UseMethod('Sensi')
Sensi.Constraint<-function(Cons,Deffunc,param){
	m<-matrix(nrow = length(param),ncol = 4)
	m[,1] = param
	for (i in seq_along(param)){
		Cons<-Deffunc(Cons,param[i])
		ca<-create(Cons)
		op<-Optim(ca)
		m[i,2:3]<-op
		m[i,4]<-op[1]/Cons$CL_MAX_STALL$q
	}
	colnames(m) = c('param','T_Wmin','W_Soptim','Clmax')
	m
}





#ca<-create(Constraint.default)
#head(ca$Out)
#plot(ca)
#Optim(ca)
#plot(ca)
#Sensi(Constraint.default,DefCA_k_AR,param=seq(from=6,to=9, by=.1))
