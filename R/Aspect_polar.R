#Aspect polar
aspect_polar<-function(value,scalar){
  if(is.null(ncol(scalar))){
    scalar=cbind(rep(0,times=length(scalar)),scalar)
  }
  radius<-value/(scalar[,2]-scalar[,1])
  return(cbind(radius,scalar))
}

plot_aspect_polar<-function(x,...) UseMethod('plot_aspect_polar')
plot_aspect_polar.numeric<-function(value,scalar,name,lty=1,pty=1,col='black',asp=1,add=F,main=NULL){
  rscale<-aspect_polar(value,scalar)
  n<-length(value)
  tta<-(1:n)*2*pi/n
  x=rscale[,1]*cos(tta);y=rscale[,1]*sin(tta)
  if(add==F) {
    plot(c(-1.1,1.1),c(-1.1,1.1),xlab='',ylab='',type='n',asp=asp,main = main)
  rx=cos(1:40/20*pi);ry=sin(1:40/20*pi)
  for(i in 1:5/5) lines(i*rx,i*ry,lty=2,col='grey')
  }
  lines(c(x,x[1]),c(y,y[1]),type='b',lty=lty,pty=pty,col=col)

  for(i in 1:n) text(cos(tta[i]),sin(tta[i]),paste(name[i],': ',rscale[i,2],'-',rscale[i,3],sep=''))
}

#plot_aspect_polar(value=c(.3,.02,6,.3),scalar = c(.6,.04,5,.2),add=T,name = c('Vv','Vh','B','VvB'))
