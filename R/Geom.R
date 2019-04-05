Geom_wing<-function(x,...) UseMethod("Geom_wing")


Geom_AR<-function(x) UseMethod("Geom_AR")
Geom_AR.wing=function(x) x$ChordT - x$ChordR
Geom_Area = function(x) UseMethod("Geom_Area")
Geom_Area.wing=function(x){
  if(x$Type == 0|x$Type == 1) return(((x$ChordR+x$ChordT)*x$Span/2))
  #if(x$type == 2) #
}
Geom_lambda.wing = function(x) x$ChordT/x$ChordR
Geom_MAC.wing = function(x){
  l=Geom_lambda.wing(x)
  (1+l+l^2)/(1+L)
}
Geom_tanSwp.numeric<-function(x,n,m,tanSwpm,AR,l){
  tanSwpm-4/AR*(n-m)*(1-l)/(1+l)
}
Geom_tanSwp.wing<-function(x,n){
  Geom_tanSwp.numeric(x=x,n=n,m=0,tanSwpm = tand(x$Sweep),A = Geom_AR.wing(x),l=Geom_lambda.wing(x))
}
