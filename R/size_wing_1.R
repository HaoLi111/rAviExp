
size_wing_simple<-function(S=1,AR=8,TR=.6,Type=0,Xf_C=.25){
b=sqrt(AR*S)#Span
MGC=S/b#Mean Geometric Cord
CordR=2*MGC/(1+TR)
CordT=CordR*TR
if(Type==0){
  Sweep=atand((CordR-CordT)*Xf_C/(b/2))
}else if(Type==1){
  Sweep=atand((CordR=CordT)*Xf_C/b)
}

re=list(Span=b,Sweep=Sweep,
        CordR=CordR,CordT=CordT,
        Type=Type,
        Xf_C=Xf_C)
class(re)='wing'
re
}


