#Polar Plot comparison


#Compare<-data.frame(aspects = c('Static Margin','Vh','Vv','B','VvB'),
          #        value = c(.2,.33,.03,6,.11),
          #         rangeLB = c(-.4,.30,.02,3,.10),
          #         rangeUB = c(.4,.60,.05,8,.20))


plot_aspects<-function(comparedt,
                       asp = 1,main = '',
                       xlab='',ylab=''){
  n = nrow(comparedt)
  r=NULL;th=NULL
  for(i in 1:n){
    r[i] = (comparedt$value[i] - comparedt$rangeLB[i])/(comparedt$rangeUB[i] - comparedt$rangeLB[i])
    th[i]=2*i*3.141593/n
  }
  r[i+1]=r[1];th[i+1]=th[1]
  tt=(0:32)/16*3.141593
  plot(c(-1,1),c(-1,1),type = 'n',xlab=xlab,ylab=ylab,main =main,asp=asp)
  for(j in (1:5)){
    lines(j/5*cos(tt),j/5*sin(tt),col ='grey',
                         lty =j)
  }
  for(j in (5:7)){
    lines(j/5*cos(tt),j/5*sin(tt),col ='red',
          lty =j-4)
  }
  points(r*cos(th),r*sin(th),type='l',lty=1,col = 'red')
  text(x=cos(th),y=sin(th),paste(comparedt$aspects,':',
                                 comparedt$value,'~',
                                 comparedt$rangeLB,'-',
                                 comparedt$rangeUB))
}
points_aspects<-function(comparedt,
                         asp = 1,col = 'blue'){
  n = nrow(comparedt)
  r=NULL;th=NULL
  for(i in 1:n){
    r[i] = (comparedt$value[i] - comparedt$rangeLB[i])/(comparedt$rangeUB[i] - comparedt$rangeLB[i])
    th[i]=2*i*3.141593/n
  }
  r[i+1]=r[1];th[i+1]=th[1]
  points(r*cos(th),r*sin(th),type='l',lty=1,col = col)
  #text(x=cos(th),y=sin(th),paste(comparedt$aspects,':',
    #                             comparedt$value,'~',
    #                             comparedt$rangeLB,'-',
     #                            comparedt$rangeUB))
}

Aspects_stability_control<-function(value){
  data.frame(aspects = c('Static Margin','Vh','Vv','B','VvB'),
                        value = value,# e.g. c(.2,.33,.03,6,.11)
                        rangeLB = c(-.4,.30,.02,3,.10),
                       rangeUB = c(.4,.60,.05,8,.20))
}
#plot_aspects(Compare)

