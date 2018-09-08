takein<-function(x,...) UseMethod('takein')
'%takein%'<-function(a,b) takein(a,b)
'%>takein>%'<-function(a,b) takein(b,a)

takein.Constraint<-function(Cons,part){
  if(class(part)=='AlphaOut'){
    o<-Optim(part)
    #Clo=as.vector(o$kmax)['Cl']
    #Cdo=as.vector(o$kmax)['Cd']
    if(class(part$Raw)=='Alpha_lin'){
      Cd_min<-part$Raw$Cd0
      k<-part$Raw$CdiF
      Cons$TURN$k<-k
      Cons$ENERGY_LEVEL$k<-k
      Cons$CLIMB$k<-k
      Cons$CRUISE_V$k<-k
      Cons$SERVICE_CEILING$k<-k

    }else{
      Cd_min=as.vector(o$Cdmin)['Cd']
      k=NA
      }


    Cons$TURN$Cd_min<-Cd_min
    Cons$ENERGY_LEVEL$Cd_min<-Cd_min
    Cons$CLIMB$Cd_min<-Cd_min
    Cons$CRUISE_V$Cd_min<-Cd_min
    Cons$SERVICE_CEILING$Cd_min<-Cd_min
    Cons$TO_DISTANCE$ClTO<-as.vector(o$Clmax)['Cl']
    Cons$TO_DISTANCE$CdTO<-as.vector(o$Clmax)['Cd']
  }
  #if(class(part)=='')
  Cons
}

#cons<- Constraint.template %takein% create(Alpha_lin.default)
#str(cons)
