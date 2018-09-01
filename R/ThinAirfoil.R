#Foil Cy and Cx calculation by thin airfoil theory.
#https://en.wikipedia.org/wiki/Airfoil#Thin_airfoil_theory
Cl_thin<-function(alpha) return(alpha/90*9.869604)

#https://www.grc.nasa.gov/www/k-12/airplane/induced.html
AR1<-function(span, area) span*span/area
AR<-function(span,cord) span/cord

#Osward efficiency coefficient- if I spelled it right.
e_type<-list(elliptical = 1,
             rectangular = .7)#do not write this as a find function!!!!!
#to use it go e_type$elliptical, if ,as vector, e_type['elliptical'] to get 1(if you remember the data then forget about this list)

e_GAAD_M1<-function(AR) 1.78*(1-.045*AR^0.68)-.64 #*GAAD P64 Method 1 (9.5.14)

Cd_induced<-function(Cl,AR, e) (Cl^2) / (3.141593 * AR * e)
Cdi<-function(alpha,Clfunction = Cl_thin,
              AR = 8,e = 1){
  (Clfunction(alpha)^2) / (3.141593 * AR * e)
}
# AR = span^2/area;area = span * cordAvg_geometric; AR = span/cordAvg_geometric
#Foil Cx Calculation
# (lift induced drag)
# required -  lift induced drag coefficient k

k<-function(AR = 8, e= 1) (3.141593 * AR * e)^(-1)

#Cl correction formulae
#Cd_thin<-function(alpha,Cdf = .05) Cdi(alpha)+Cdf

#createAlpha
createAlpha<-function(alpha = seq(from = -5, to = 10, by = .1),
                      Clfunction = Cl_thin,#Thin airfoil theory
                      Cdfunction = Cdi,#all Cl and Cd witten with respect to Alpha (or set default for other parameters)
                      plot = T,
                      return = T){
  Cl<-Clfunction(alpha)
  Cd<-Cdfunction(alpha)
  alpha<-data.frame(alpha)
  print(Clfunction)
  print(Cdfunction)
  alpha$Cl = Cl
  alpha$Cd = Cd
  alpha$k = (Cl/Cd)
  Raw = list(Clfunc = Clfunction,Cdfunc = Cdfunction)
  class(Raw)='func'
  alpha = list(Raw = Raw,Out = alpha)
  class(alpha) = 'AlphaOut'
  if(plot == T) plotAlpha(alpha)
  if(return ==T) return(alpha)
}
#Cl_thin(seq(from = -5, to = 10, by = .1))
plotAlpha<-function(alpha){
  layout(matrix(1:4,2))
  plot(alpha$alpha,alpha$Cl,type = 'l',main = "Cl ~ alpha",xlab = 'alpha',ylab = 'Cl')
  abline(h = fetchAlpha_kmax(alpha)['Cl'],col = 'green')# Cl at max lift to drag ratio
  abline(v = fetchAlpha_kmax(alpha)['alpha'],col = 'green')
  plot(alpha$alpha,alpha$Cd,type =  'l',main = "Cd ~ alpha",xlab = 'alpha',ylab = 'Cd')
  abline(h = fetchAlpha_kmax(alpha)['Cd'],col = 'green')
  abline(v = fetchAlpha_kmax(alpha)['alpha'],col = 'green')
  plot(alpha$alpha,alpha$k,type = 'l',main = "k (Cl/Cd) (d/h)max ~ alpha",xlab  = 'alpha',ylab = 'k (Cl/Cd)')
  plot(alpha$Cd,alpha$Cl,type = 'l',main = "Cl ~ Cd",xlab = 'Cd',ylab= 'Cl')
  abline(h = fetchAlpha_kmax(alpha)['Cd'],col = 'green')
  abline(v = fetchAlpha_kmax(alpha)['Cl'],col = 'green')
  layout(matrix(1))
}
#createAlpha(plot = F)
#createAlpha(plot = T)

#specify by airfoil <- createAlpha(...)

#fetchAlpha
#fetchAlpha()
fetchAlpha<-function(alpha,aspect = c(T,T,T,T)){
  subset(alpha,Cd == min(Cd)|k == max(k)|Cl == max(Cl))
}
#fetchAlpha(createAlpha())
fetchAlpha_Clmax<-function(alpha,aspect = c(T,T,T,T)){
  subset(alpha,Cl == max(Cl),aspect)
}
#fetchClmax(createAlpha())
fetchAlpha_Cdmin<-function(alpha,aspect = c(T,T,T,T)){
  subset(alpha,Cd == min(Cd),aspect)
}
#
fetchAlpha_kmax<-function(alpha,aspect = c(T,T,T,T)){
  subset(alpha,k == max(k),aspect)
}

fetchAlpha_Cl<-function(alpha,Cl){
  require('MFVN')
  DFRead2(alpha,target = Cl, independent = 2,dependent=1:4)
}

Optim.AlphaOut<-function(AO){
	list(kmax = fetchAlpha_kmax(AO$Out),
	Cdmin = fetchAlpha_Cdmin(AO$Out),
	Clmax = fetchAlpha_Clmax(AO$Out))
}


Alpha_lin.default<-list(Cla = .1,alpha0 = -5,CdiF = 1 / (3.141593 * 8 * 1),Cd0 = .02)
class(Alpha_lin.default) ='Alpha_lin'
create.Alpha_lin<-function(AAR,alpha = seq(from = -5, to = 10, by = .1)){
  Cl = AAR$Cla * (alpha - AAR$alpha0)
  Cd = AAR$Cd0 + Cl*Cl*AAR$CdiF
  k = Cl/Cd
  re = list(Raw = AAR,Out = data.frame(alpha =  alpha,Cl = Cl,Cd = Cd,k = k))
  class(re) = 'AlphaOut'
  re
}
#----------------------
#Correct Cla an
Alpha_AR_corr.default<-list(AR = 8,SweepQuarter = 10,Cla = .1,Ma = .1)
class(Alpha_AR_corr.default)<-'Alpha_AR_corr'


create.Alpha_AR_corr<-function(AAR,alpha = seq(from = -5, to = 10, by = .1)){
  #1. generate Alpha0
  # TO BE DEVELOPED Alpha0 =
  #2. generate Cla
  Cla = Cla_corr(AAR$AR,AAR$SweepQuarter,Cla = AAR$Cla,Ma = AAR$Ma)
  #3. generate Cl
  Cl = Cla*(alpha-Alpha0)
  #4. generate Cd0w
  # TO BE DEVELOPED Cdow =
  #5. generate Cdiw
  #TO BE DEVELOPED Cdiw =
  #6. generate Cd
  Cd = Cl^2/Cdiw + Cd0w

}

print.Alpha_lin<-function(x){
  print(paste('Alpha_lin:Cla=',x$Cla,'alpha0=',x$alpha0,'CdiF=',x$CdiF,'Cd0=',x$Cd0))
}
print.AlphaOut<-function(AlphaOut){
	print('Generated from $Raw')
	print(AlphaOut$Raw)
	print('Out : ')
	print(head(AlphaOut$Out))
	print('......')
	print(tail(AlphaOut$Out))
}
lines.AlphaOut<-function(AlphaOut,type='l'){
	if(type == 'l') plotAlpha(AlphaOut$Out)

}
plot.AlphaOut<-function(A){
	require('scatterplot3d')
	scatterplot3d(x=A$Out[,'Cl'],y=A$Out[,'Cd'],z=A$Out[,'k'],highlight.3d=T,xlab = 'Cl',ylab='Cd',zlab='k')
}

Convert.Xfoil<-function(x){
  re<-list(Raw = deparse(substitute(x)),Out = data.frame(alpha = x[,'alpha'],
                                                         Cl = x[,'CL'],
                                                         Cd = x[,'CD'],
                                                         k = x[,'CL']/ x[,'CD'],
                                                         Cm = x[,'CM'],
                                                         Cdp = x[,'CDp'],
                                                         XtrT=x[,'Top_Xtr'],
                                                         XtrB=x[,'Bot_Xtr']))
  class(re)='AlphaOut'
  re
}
