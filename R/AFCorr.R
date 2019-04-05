#From Springer Fundamental of Aircraft flight machanics
#Translational
AFCorr_Cla<-function(AR,tanSwphc,M,k) (3.141593*AR/(1+sqrt(1+(AR/2/k)^2*(1+tanSwphc^2-M^2))))
#AFCorr_Cla(8,.2,.25,.1/(6.28+4.7*.1))
AFCorr_Cla_k<-function(Clam0,Clatheory) Clamo/Clatheory

#When Cla0 is small Cp corr can be used to correct Cla
#Correction for Cp
#See
#https://www3.nd.edu/~atassi/Teaching/ame%2060639/Notes/compressible.pdf
Cp_PG=function(Cp0,Ma) Cp0/sqrt(1-Ma^2)# Prandtl_Glauert
Cp_KT=function(Cp0,Ma) Cp0/(sqrt(1-Ma^2)+Cp0*(Ma^2/(1+sqrt(1-Ma^2)))/2)# Karman_Tsien
Cp_L=function(Cp0,Ma,Gamma=0) Cp0/(sqrt(1-Ma^2)+(Ma^2*(1+((Gamma-1)/2)*Ma^2))/(2*sqrt(1-Ma^2))*Cp0)# Laitone

#Need Ma
Cdf = function(x,...) UseMethod('Cdf')
Cdf.vector = function(fk,S) 1.1/S*as.numeric(sum(fk))

Cfk_Rek<-function(Rek) 0.455/((log10(Rek))^2.58)
Re_def=function(rho,v,l,mew) rho*v*l/mew
#need Rho(h) Mew(h)
Re_v_h<-function(v,l,h) Rho(h)*v/Mew(h)
Cfk_l_h<-function(v,l,h) .455/((log10(Re_v_h(v,l,h)))^2.58)
CFk<-function(M) (1+M^2*.2)^(-.467)
#need IF()
#IFk<-data.frame(Airplane_Component = c('Body','Wing','Horizontal_Tail','Vertical_Tail',
 #                                      'Wing_Nacelles','Body_Nacelles','Tip_Tanks'),
  #              k = c('B','W','H','V','N','N','T'),
   #             IF = c(1.20,1.20,1.10,1.10,1.30,1.50,1.25))
#
FFw=function(t_c) 1+1.6*(t_c)+100*(t_c)^4
FFb=function(l_d) 1+60/(l_d)^3+.0025*l_d
FFn=function(l_d) 1+.35/(l_d)
#need Swet() maybe Geom_Swet for wet surface area estimation

#Cdw
Cdw<-function(Ma,g1,g2,Cl) 29.2*(M-(g1-g2*Cl))^3#need update
Cdw_g1<-function(Swpps,Swpmt) NA#need update

#

#wrap until Cl ~ M,a, Cd~M,a for 1 concept
#for 1 concept ply Cl,Cd~a / M or mply a,M
ply_Cp<-function(x,M,alpha,h){}#x -  the concept; M - Mach,a;h -height (4 deg of freedom)
#Disclude a to find gradient, store into a list
