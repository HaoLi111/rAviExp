size_wing_simple<-function(S=1,AR=8,TR=.6,Type=0,Xf_C=.25){
b=sqrt(AR*S)#Span
MGC=S/b#Mean Geometric Chord
ChordR=2*MGC/(1+TR)
ChordT=ChordR*TR
if(Type==0){
  Sweep=atand((ChordR-ChordT)*Xf_C/(b/2))
}else if(Type==1){
  Sweep=atand((ChordR-ChordT)*Xf_C/b)
}

re=list(Span=b,Sweep=Sweep,
        ChordR=ChordR,ChordT=ChordT,
        Type=Type,
        Xf_C=Xf_C)
class(re)='wing'
re
}


