size_concept_simple<-function(AR=8,
TR=.6,
S=1,
ARh=6,
TRh=1,
ARv=3,
TRv=1,
Vh=.3,
Vv=.02,
Xf_C=.25,#of main wing
l=.8,#lever arm tail
xWM=.3)
{
Xnp_C=Xf_C + (1+2/AR)/(1+2/ARh)*(1-4/(AR+2))*Vh
#assume cg is at np
#Solve for Wings and Stabs
Xcg_C=Xnp_C
WM=size_wing_simple(S,AR,TR,Type=0,Xf_C = Xf_C)
m<-MAC(WM)
ShLh=m$ChordAvg*S*Vh
SvLv=WM$Span*S*Vv

Sh=ShLh/l
Sv=SvLv/l
WH=size_wing_simple(Sh,ARh,TRh,Type=0,Xf_C=Xf_C)
WV=size_wing_simple(Sv,ARv,TRv,Type=1,Xf_C=Xf_C)
mh<-MAC(WH)
mv<-MAC(WV)
#position Wings and Stabs
WM$x=xWM;WM$y<-WM$z<-0
Xnp=xWM+m$xMAC+Xnp_C*m$ChordAvg
xT<-Xnp+l
WH$x<-xT-mh$AF
WV$x<-xT-mv$AF
WH$y<-WH$z<-WV$y<-WV$z<-0

concept<-list(WM=WM,WV=WV, WH=WH)
class(concept)='conventionalConcept'
return(concept)
#concept$fuselage<-fuselage.default
#plotxy(concept)
#return(list(concept=concept,
           # Xnp=Xnp))
}
