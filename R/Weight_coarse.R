

create.Weight_coarse<-function(wt){
  
  m_gross=wt$PAYLOAD$m/wt$PAYLOAD$mFrac
  FG_gross=m_gross*9.803
  FT_cruise=wt$T_W*FG_gross
  Power_T<-FT_cruise*wt$v
  Power=Power_T+wt$PAYLOAD$P_draw
  Time=wt$range/wt$v
  Energy=Time*Power
  
  re<-list(m_gross=m_gross,
           FG_gross=FG_gross,
           FT_cruise=FT_cruise,
           Power_T=Power_T,
           Time=Time,
           Energy=Energy)
  class(re)='Weight_coarseOut'
  re
}
