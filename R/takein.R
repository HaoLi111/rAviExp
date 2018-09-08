takein<-function(x,...) UseMethod('takein')
'%takein%'<-function(a,b) return(takein(a,b))
'%>takein>%'<-function(a,b) return(takein(b,a))

takein.Constraint<-function(Cons,part){
  if(class(part)=='AlphaOut'){
    o<-Optim(part)
    #Clo=as.vector(o$kmax)['Cl']
    #Cdo=as.vector(o$kmax)['Cd']
    if(class(part$Raw)=='Alpha_lin'){
      assign('Cd_min',part$Raw$Cd0)
      assign('k',part$Raw$CdiF)
      Cd_min<-as(Cd_min,'numeric')
      k<-as(k,'numeric')
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
    Cons$TO_DISTANCE$ClTO<-as(o$Clmax['Cl'],'numeric')
    Cons$TO_DISTANCE$CdTO<-as(o$Clmax['Cd'],'numeric')
  }
  #if(class(part)=='')
  Cons
}

#cons<- Constraint.template %takein% create(Alpha_lin.default)
#str(cons)
