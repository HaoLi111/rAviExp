#Stability and control simplified

SC_Vh=function(Sh,Lh,S,C) Sh*Lh/S/C

SC_B=function(lv,b,Gamma,Cl) lv/b*Gamma/Cl

SC_simp<-function(x,...) UseMethod("SC_simp")
SC_simp.conventionalConcept=function(x,Xcg,Xnp = NULL,gamma.include =F,Cl = NA,Gamma=NA){
  MACM = MAC(x$WM);MACH= MAC(x$WH);MACV=MAC(x$WV)
  #absolute position of m,h,v
  Pos = list(xM = MACM$AF+x$WM$x,
             xH = MACH$AF+x$WH$x,
             xV = MACV$AF+x$WV$x)
  Lever = list(L = Xcg - Pos$xM,
               LH= Xcg - Pos$xH,
               LV= Xcg - Pos$xV)
  Vh = Geom_Area(x$WH)*Lever$LH / Geom_Area(x$WM) / MACM$ChordAvg#Sh Lh/S/c
  Vv = Geom_Area(x$WV)*Lever$LV / Geom_Area(x$WM) / as.numeric(x$WM$Span)#MACM$ChordAvg
  if (is.null(Xnp)){
    Xnp_OCW_est<-MACM$ChordAvg*(.25+(1+2/Geom_AR(x$WM))/(1+2/Geom_AR(x$WH)))*(1-4/(2+Geom_AR(x$WM))*Vh)+x$WM$x
    #message(paste0('No Xnp Detected, using estimation from OCW Lab8 Notes P4',Xnp_OCW_est))
    Xnp = Pos$xM# Replace the estimated neutral point by OCW by the aerodynamic focus of the main wing
  }
  SM =(Xcg - Xnp)/ MACM$ChordAvg#;message('OK')#Static Margin
  #Nondimensionization (torque d(T)/d(alpha) / (.5*Cl*rho*v^2) )
  M_de<-Lever$L  / MACM$ChordAvg + Vh#l/b - St*L1/(Sw*b) P67 fuyangwendingxishu
  Coef = cbind(Vh,Vv,SM,M_de)

  if(gamma.include==T){
    B = Lever$LV / x$WM$Span * Gamma / Cl#Gamma in Deg Blaine Rowdon Factor of spiral stability
    VvB = Vv*B
    Coef = cbind(Coef,B)
    Coef = cbind(Coef,VvB)
  }

  Raw = list(Xcg=Xcg,Xnp=Xnp,Xnp_2 = Xnp_OCW_est)
  re = list(From =deparse(substitute(x)),Raw = Raw,Pos = Pos,Lever = Lever,Coef=Coef)
  class(re) = 'SCSimpOut'
  re
}

print.SCSimpOut<-function(x){
  print(paste('Simplified Stability and Control Analysis for',x$From))
  print(paste('Center of Gravity (Xcg):',x$Raw$Xcg,'|Neutral Point(Xnp):',x$Raw$Xnp))
  print('...Coefficients:')
  print(x$Coef)
}
