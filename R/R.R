#R


R_Fi<-function(v,R,g=9.803) atand(v^2/g/R)
Fi_R<-function(v,Fi,g=9.803) v*v/g/tand(Fi)

Fi_beta_est<-function(Gamma,Cl,Cla,
                  b,R,Fi){
  if(!Gamma==0){
    return(rtd(1/Gamma*Cl/Cla*b/2/R*cosd(fi)))
    #b,Gamma in Deg
  }else{
    message('Infeasible estimation, Gamma=0')
  }
}

Fi_n=function(Fi) 1-cosd(Fi)
#Relative Spanwise Velocity
R_u=function(y,v,R) v*(1-y/R)#y towards inside
R_Qdv<-function(b,v,R) b*v/2/R#velocity difficit from 2 lift center estimators ((1/4) (b/2))
R_t<-function(v,R) 2*3.141593

#NEED UPDATE
Fi.default=list(v=10:20,Fi=1:20)
class(Fi.default)='Fi'
create.Fi<-function(x,include_l_usage = T){
  Fi = x$Fi; v = x$v
  re=list(Raw=x,include_l_usage=include_l_usage)
  if(length(Fi)==1){
    re$Out =NA
    for(i in seq_along(v)){

    }
  }
}

