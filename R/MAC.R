#Naming refers to


#  Root Cord (A):
#MAC Graphic
#  Tip Cord (B):
#  Sweep Distance (S):
#  Half Span (Y):
#  C  of Gravity (CG):
#Sweep Distance @ MAC (C) =
#Mean Aerodynamic Cord (MAC) =
#MAC Distance from Root (d) =
#http://www.nasascale.org/p2/wp-content/uploads/mac-calculator.htm
#
#MAC
MAC_S<-function(span,sweep) span * tand(sweep)/2
MAC_C<-function(S,A,B) (S*(A+2*B)) / (3*(A+B))#
MAC_MAC<-function(A,B) A-(2*(A-B)*(0.5*A+B) / (3*(A+B)))
MAC_d<-function(A,B,Y)  (2*Y*(0.5*A+B)) / (3*(A+B))
#MAC_B.P.<-function(CG,C,MAC) ((CG-C) / MAC)
MAC_AF<-function(B.P.,MAC,C) C + B.P.* MAC

MAC<-function(x,...) UseMethod('MAC')


MAC.wing<-function(wing,B.P=NA){
  if(is.na(B.P)){
    try(B.P<-wing$Xf_C)
    message('B.P is missing from call, Extracting from wing')
  }
  if(wing$Type==0){
    re<-list(
      xSweep = MAC_S(wing$Span,wing$Sweep),#Sweep Distance @ tip
    xMAC  = MAC_C(MAC_S(wing$Span,wing$Sweep),wing$CordR,wing$CordT),#Sweep Distance @ MAC
    CordAvg = MAC_MAC(wing$CordR,wing$CordT),
    yMAC = MAC_d(wing$CordR,wing$CordT,wing$Span/2),
    AF=MAC_AF(B.P,MAC_MAC(wing$CordR,wing$CordT), MAC_C(MAC_S(wing$Span,wing$Sweep),wing$CordR,wing$CordT)))
    re$Type=0
    class(re)<-c('MAC','list')
    re
  }else if(wing$Type==1){
    wing$Span<-wing$Span*2
    wing$Type<-0
    #wing$
    rr=MAC(wing)
    rr$Type=1
    return(rr)
  }else if(wing$Type==2){

  }
}


#MAC_CG<-function(B.P.,MAC,C) C + B.P.* MAC
#afprop<-MAC(wing.default,B.P = .25)


pointsxy.MAC<-function(afprop,xf=0){
  lines(c(xf+afprop$xMAC,xf+afprop$xMAC+afprop$CordAvg),c(afprop$yMAC,afprop$yMAC),type='l',col='red')
  points(xf+afprop$AF,afprop$yMAC,col='red')
}

#w = list(x=0,CordR = .5,CordT = .3,Sweep = 12,Span = 1.5)
#class(w) = 'wing'

#MAC.wing(w,Xcg = .21)
#
plotxy<-function(x,...) UseMethod('plotxy')
pointsxy<-function(x,...) UseMethod('pointsxy')


plotxy.concept<-function(concept,asp = 1){
    plot(c(0,FU$Length),c(0,FU$Length),type = 'n',asp = asp)
    plotxy.canard<-function(concept){
    pointsxy(WM)
    pointsxy(WC)
    }
    plotxy.canard2v<-function(concept){
    pointsxy(WM)
    pointsxy(WC)
    }
    plotxy.conventional<-function(concept){
    pointsxy(WM)
    pointsxy(WH)
    }
    plotxy.conventional2v<-function(concept){
    pointsxy(WM)
    pointsxy(WH)
    }
    plotxy.aerobatics<-function(concept){
    pointsxy(WM)
    pointsxy(WH)
    }
    plotxy(concept)
}

pointsxy.concept<-function(concept){
        plotxy.canard<-function(concept){
    pointsxy(WM)
    pointsxy(WC)
    }
    plotxy.canard2v<-function(concept){
    pointsxy(WM)
    pointsxy(WC)
    }
    plotxy.conventional<-function(concept){
    pointsxy(WM)
    pointsxy(WH)
    }
    plotxy.conventional2v<-function(concept){
    pointsxy(WM)
    pointsxy(WH)
    }
    plotxy.aerobatics<-function(concept){
    pointsxy(WM)
    pointsxy(WH)
    }
    pointsxy(concept)
}

#pointsxy.MAC<-function(MAC,x=0){
 #   points(c(x + MAC$D_Sweep_atMAC,x+ MAC$D$Sweep_atMAC +MAC$CordAvg),c(MAC$yMAC,MAC$yMAC),type = 'l',col = 'red')
#}

extend<-function(x,...) UseMethod('extend')
