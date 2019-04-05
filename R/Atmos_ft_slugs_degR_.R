#Atmosphere
#
#NEED CONVERSION BETWEEN FT and M
#Standard atmos
Te_h_ft<-function(h) 518.69-3.5662E3*h
p_h_ft<-function(h) 1.1376E-11*((Te_h_ft(h))^5.2560)
Rho_h_ft<-function(h) 6.6277E-15*(Te_h_ft(h)^4.2560)
Mew_Te<-function(Te) ((2.27E-8 * T^(3/2))/(Te+198.6))
#Exponential atmos
