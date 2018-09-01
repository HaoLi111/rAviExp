#load analysis
#see also : V-G diagram
#
#input

#at certain rho (assume H )

load.default = list(v=seq(from = .1,to=50,by=.1),
            Cl_max= 1.2,
            Cl_min = -.7,
            H = 0,
            FG = 9.803*3,
            n_max = 6,
            n_min=-6,
            v_max = 30,
            q_max = 30^2/2*Rho(H=0),
            S=.8)

load.default=CompleteRho(load.default)
load.default$q<-1/2*load.default$rho*load.default$v^2
class(load.default) = 'nAnalysis'

#CompleteQ fron H and rho

#Cl curve
LOAD_CL<-function(x,...) UseMethod('LOAD_CL')
LOAD_CL.numeric<-function(q,Cl,FG){
  q*Cl/FG
}

LOAD_CL.nAnalysis<-function(load){
  nmax<-numeric(length(load$q));nmin<-nmax
  for(i in seq_along(load$q)){
  nmax[i]<-LOAD_CL(load$q[i],load$Cl_max,load$FG)
  nmin[i]<-LOAD_CL(load$q[i],load$Cl_min,load$FG)
  }
  r=cbind(load$q,nmax,nmin)
  colnames(r) = c('q','nmax','nmin')
  r
  #try(r=cbi)
}



create.nAnalysis<-function(raw){
  o=LOAD_CL.nAnalysis(raw)
  re=list(Raw=raw,Out=o)
  class(re)='nAnalysisOut'
  re
}

lines.nAnalysisOut<-function(x,type='v',xlim = NULL, ylim = NULL){
  if(type=='v'){
    matplot(x$Raw$v,x$Out[,c('nmax','nmin')],xlab='v',ylab ='n',
            main = paste('n Analysis of',deparse(substitute(x)),sep=' '),type='l',xlim=xlim,ylim=ylim)
    try(abline(v=x$Raw$v_max,lty=2))
    try(abline(v=x$Raw$v_min,lty=2))
    try(abline(h=x$Raw$n_max,col = 'red',lty=3))
    try(abline(h=x$Raw$n_min,col='red',lty=3))
  }else if(type=='q'){
    matplot(x$Out[,'q'],x$Out[,c('nmax','nmin')],xlab='q',ylab ='n',
            main = paste('n Analysis of',deparse(substitute(x)),sep=' '),type='l',xlim=xlim,ylim=ylim)
    try(abline(v=x$Raw$q_max,lty=2))
    try(abline(v=x$Raw$q_min,lty=2))
    try(abline(h=x$Raw$n_max,col = 'red',lty=3))
    try(abline(h=x$Raw$n_min,col='red',lty=3))
  }
}

Optim.nAnalysisOut<-function(x,Cl_kmax=.2){
  for(i in x$Raw$q) n<-LOAD_CL(load$q[i],load$Cl_max,load$FG)
  v_optim=which(n-1<1e-6)
  list(v_optim,n)
}

print.nAnalysisOut<-function(x){
  print(str(x$Raw))
  print(head(x$Out))
  print('......')
  print(tail(x$Out))
}

Ext<-function(x,...) UseMethod('Ext')
#Ext.nAnalysisOut<-function(x){
 # try(ext = list(v_min=))
#}

#(ld<-create(load.default))
#lines(ld)
#lines(ld,'q')
#(o=Optim(ld))
