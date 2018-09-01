#Mapply_lift.numeric<-function()

Mapply_lift_S<-function(alpha=seq(from=0,to=10,by=.1),v=seq(from=10,to=30,by=.1),S,rho=1.225,Clfunc=Cl_thin){
  l=matrix(NA,length(v),length(alpha))
  CL=sapply(alpha,Clfunc)
  smp=rho*S*CL
  for(V in seq_along(v)){
      l[V,]=smp*v[V]^2/2
  }
  l
}

Mapply_drag_S<-function(alpha=seq(from=0,to=10,by=.1),v=seq(from=10,to=30,by=.1),S,rho=1.225,Cdfunc=function(Cl) Cdi(Cl)+.02){
  d=matrix(NA,length(v),length(alpha))
  CD=sapply(alpha,Cdfunc)
  smp=rho*S*CD
  for(V in seq_along(v)){
    d[V,]=smp*v[V]^2/2
  }
  d
}

