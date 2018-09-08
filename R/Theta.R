
#climbAngle = -10:10#Climbangle negative for gliding
FD = function(Cd,rho,S,v) .5*Cd*rho*S*v^2
#Cd = k* Cl


#assume all Cl and Cd is used (alpha stays constant)
#------------------------------------------------------------------
v_motion_LEAST<-function(Cl,rho,S,FG) sqrt(2*FG/(Cl*rho*S))

v_motion_LEAST_ANGLE<-function(angle,Cl,rho,S,FG) sqrt((2*FG)/(Cl*rho*S))*sqrt(cosd(angle))#for both gliding,climbing and horz flying

vy_motion_vy_ANGLE<-function(angle,Cl,rho,S,FG) sqrt((2*FG)/(Cl*rho*S))*sqrt(cosd(angle))*sind(angle)

vy_motion_vy_MAX<-function(Cl,rho,S,FG) sqrt((2*FG)/(Cl*rho*S))*sqrt(cosd(atand(sqrt(2))))*sind(atand(sqrt(2)))


#angle_vy_MAX = atan(sqrt(2))*180/3.141593


F_motion_LEAST_ANGLE<-function(angle,k,FG) FG*(sind(angle) + 1/k*cosd(angle))

angle_Thrust_MAX<-function(k) atand(k)

E_motion_LEAST_ANGLE<-function(angle,k,l_climb,FG) FG*l_climb*(sind(angle) + 1/k*cosd(angle))

ita_h_l_climb_ANGLE<-function(angle,k) sind(angle) + cosd(angle)/k

P_motion_LEAST_ANGLE<-function(angle,Cl,Cd,rho,S,FG){
  k=Cl/Cd
  (FG*(sind(angle) + 1/k*cosd(angle)))*(sqrt((2*FG)/(Cl*rho*S))*sqrt(cosd(angle)))
}



#v_motion_GLIDING_ANGLE<-function(climbAngle,Cl,rho,S,FG) sqrt(cosd(climbAngle)*(2*FG)/(Cl*rho*S))

ita_l_h_glide_ANGLE<-function(k) k
#------------------------------------------------------------------------
state.default= list(Cl = .45,Cd = .02,FG = 15,rho = 1.213,S = .7,H = 100)
state.template = list(Cl = NA,Cd = NA,FG = NA,rho = NA,S = NA,H = NA)


PowerPlant.default = list(F_max = 7,P_max = 95)
PowerPlant.template = list(F_max = NA,P_max = NA)

theta.default =list(theta=seq(from=0,to=65,by=.1),state = state.default,Pp=PowerPlant.default)
theta.template = list(theta=NA,state = state.template,Pp=PowerPlant.template)
class(theta.default) ='Theta'
class(theta.template)='Theta'
#------------------------------------------------------------------------


motion_LEAST_v_ANGLE<-function(angle,state = state.default){
	data.frame(angle = angle,
		v = v_motion_LEAST_ANGLE(angle,state$Cl,state$rho,state$S,state$FG),
		vx = v_motion_LEAST_ANGLE(angle,state$Cl,state$rho,state$S,state$FG)*cosd(angle),
		vy = v_motion_LEAST_ANGLE(angle,state$Cl,state$rho,state$S,state$FG)*sind(angle),
		Thrust = F_motion_LEAST_ANGLE(angle,state$Cl/state$Cd,FG=state$FG),
		Fl=state$Cl/2*state$rho*state$S*v_motion_LEAST_ANGLE(angle,state$Cl,state$rho,state$S,state$FG),
		Fd=state$Cd/2*state$rho*state$S*v_motion_LEAST_ANGLE(angle,state$Cl,state$rho,state$S,state$FG),
		Power = P_motion_LEAST_ANGLE(angle,state$Cl,state$Cd,state$rho,state$S,state$FG))
}#at Cl, Cd and alpha, the least v required to maintain climbing wtih acceleration = (0,0,0) and v, P FG

create.Theta<-function(Raw){
  re=list(Raw=Raw,Out=motion_LEAST_v_ANGLE(angle =Raw$theta,state=Raw$state))
  class(re)='ThetaOut'
  re
}

lines.ThetaOut<-function(o){
  layout(matrix(1:4,2,2))
  #1
  m=o$Out
  matplot(m[,1],m[,3:4],xlab ='Angle (DEG)',ylab = 'Vx , Vy',type = 'l',main = 'Least Vx , Vy ~ ClimbAngle')
  legend(x=max(m[,1])/5,y = max(m[,3:4]),legend =c('Vx' ,'Vy'),
         col=1:2,lty=1:2,bty='n')
  try(abline(v = m[which.max(m[,'vy']),1],col='green',lty=3))
  try(abline(h = max(m[,'vy']),col='green',lty=3))
  #2

  matplot(m[,'angle'],m[,c('Thrust','Power')],type='l',
          xlab ='Angle (DEG)',ylab = 'F_T (Thrust) , P (Power)',
          main = 'F_T (Thrust) , P (Power) ~ Angle')
  legend(x=max(m[,1])/5,y = max(m[,c('Thrust','Power')]),legend =c('Thrust','Power'),
         col=1:2,lty=1:2,bty='n')
  try(abline(v = m[which.max(m[,'Thrust']),1],col='purple',lty=3))
  try(abline(v=m[which.max(m[,'Power']),1],col='purple',lty=3))
  try(abline(h = max(m[,'Thrust']),col='purple',lty=3))
  try(abline(h = max(m[,'Power']),col='purple',lty=3))

  try(abline(h=o$Raw$Pp$P_max,col='brown',lty=5))
  try(abline(h=o$Raw$Pp$F_max,col='brown',lty=5))

  #3
  plot(m[,'vx'],m[,'vy'],type='l',
       xlab ='Vx',ylab = 'Vy',
       main = 'Vy ~ Vx')

  try(abline(v = m[which.max(m[,'Thrust']),'vx'],col='purple',lty=3))
  try(abline(v=m[which.max(m[,'Power']),'vx'],col='red',lty=3))
  try(abline(h= m[which.max(m[,'Thrust']),'vy'],col='purple',lty=3))
  try(abline(h=m[which.max(m[,'Power']),'vy'],col='red',lty=3))
  #4
  plot(m[,'Thrust'],m[,'Power'],type='l',main='Power ~ Thrust',xlab = 'F_T (Thrust)',ylab='P (Power)')
  try(abline(v = o$Raw$Pp$F_max,col='brown',lty=5))
  try(abline(h = o$Raw$Pp$P_max,col='brown',lty=5))
  layout(matrix(1,1))
  legend(x=max(m[,'Thrust'])/5,y = max(m[,'Power'])*4/5,legend =c('P ~ F','limit'),
         col=c('black','brown'),lty=1:2,bty='n')
}


#------------------------------------------------------------------------

analyse<-function(x,...) UseMethod('analyse')
analyse.state<-function(state){
	k = state$Cl/state$Cd

}



analyse.Gliding_State<-function(state= state.default){
	list(GlidingRatioMax = state$Cl / state$Cd,
		angle = -atand(1/(state$Cl / state$Cd)),
		v = v_motion_LEAST_ANGLE(-atand(1/(state$Cl / state$Cd))),
		vx = v_motion_LEAST_ANGLE(-atand(1/(state$Cl / state$Cd))) * cosd(-atand(1/(state$Cl / state$Cd))),
		vy = v_motion_LEAST_ANGLE(-atand(1/(state$Cl / state$Cd))) * sind(-atand(1/(state$Cl / state$Cd)))
		)
}

#Fmax_properties<-function(state = state.default){
#	list(angle_Fmax = atand(state$Cl/state$Cd),
#		P_Fmax)
#}

#analyse.PowerPlant_State<-function(pp = )
