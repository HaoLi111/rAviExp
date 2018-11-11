Geom_wing<-function(x,...) UseMethod("Geom_wing")


Geom_AR<-function(x) UseMethod("Geom_AR")
Geom_AR.wing=function(x) x$ChordT - x$ChordR
Geom_Area = function(x) UseMethod("Geom_Area")
Geom_Area.wing=function(x){
  if(x$Type == 0|x$Type == 1) return(((x$ChordR+x$ChordT)*x$Span/2))
  #if(x$type == 2) #
}
