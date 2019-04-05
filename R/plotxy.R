plotxy<-function(x,...) UseMethod('plotxy')

plotxy.wing<-function(wing,add=F,col='black',lty=1){
  if(add==T){
    pointsxy.wing(wing,col=col,lty=lty)
  }else{
    #if(missing(wing$Dihedral)) wing$Dihedral=0
  span<-wing$Span/2
  x=wing$x;y=wing$y;z=wing$z
  front<-c(x,y,z)
  #dim(front) = c(1,3)
  rear<-c(x+wing$ChordR,y,z*wing$Dihedral)
  #dim(rear) = c(1,3)
  frontT<-c(x+span*sind(wing$Sweep),y+span*cosd(wing$Sweep),z+span*cosd(wing$Dihedral))
  #dim(frontT) = c(1,3)
  rearT<-c(x+span*sind(wing$Sweep)+wing$ChordT,y+span*cosd(wing$Sweep),z+span*cosd(wing$Dihedral))
  #dim(rearT) = c(1,3)
  pointCloud<-rbind(as.numeric(front)[1:2],as.numeric(frontT)[1:2],as.numeric(rearT)[1:2],as.numeric(rear)[1:2])
  plot(pointCloud[,1],pointCloud[,2],xlab='x',ylab='y',type='l',asp=1,col=col,lty=lty)
  }
}

pointsxy.wing<-function(wing,col='black',lty=1){
  #if(missing(wing$Dihedral)) wing$Dihedral=0
  span<-wing$Span/2
  x=wing$x;y=wing$y;z=wing$z
  front<-c(x,y,z)
  rear<-c(x+wing$ChordR,y,z*wing$Dihedral)
  frontT<-c(x+span*sind(wing$Sweep),y+span*cosd(wing$Sweep),z+span*cosd(wing$Dihedral))
  rearT<-c(x+span*sind(wing$Sweep)+wing$ChordT,y+span*cosd(wing$Sweep),z+span*cosd(wing$Dihedral))
  pointCloud<-rbind(front,frontT,rearT,rear)
  points(pointCloud[,1],pointCloud[,2],xlab='x',ylab='y',type='l',asp=1,col=col,lty=lty)
}

plotxy.fuse<-function(fuse,add=F,col='black',lty=1){

  if(add==T){
    points(fuse$x,fuse$r,col=col,type='l',lty=lty)
  }else{
    plot(fuse$x,fuse$r,xlab='x',ylab='y',type='l',asp=1,col=col,lty=lty)
    }
}
pointsxy.fuse<-function(fuse,col='black',lty=1){
    points(fuse$x,fuse$r,col=col,type='l',lty=lty)
}

#plotxy(fuselage.default,add=T)
plotxy.conventionalConcept = function(concept){
  #range of plotting
  plot(c(0,max(concept$fuselage$x)),c(0,max(concept$WM$Span/2)),type='n',asp=1,
       xlab = 'x',ylab = 'y')
  pointsxy(concept$fuselage)
  pointsxy(concept$WM)
  pointsxy(concept$WH)
  pointsxy(concept$WV,col = 'blue',lty=2)
  try(pointsxy(MAC(concept$WM),xf=concept$WM$x))
  try(pointsxy(MAC(concept$WH),xf=concept$WH$x))
  try(pointsxy(MAC(concept$WV),xf=concept$WV$x))
}

