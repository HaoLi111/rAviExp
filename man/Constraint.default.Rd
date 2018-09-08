\name{Constraint.default}
\alias{Constraint.default}
\docType{data}
\title{
Default Classified list for constraint analysis
}
\description{
Restrictions of a project.
The parameters mainly restrict kinetic properties.
}
\usage{data("Constraint.default")}
\format{
  The format is:\cr
List of 8\cr
 $ W_S            : num [1:451] 5 5.1 5.2 5.3 5.4 5.5 5.6 5.7 5.8 5.9 ...(List of W_S, wing loading to calculate)\cr
 $ TURN           :List of 6 (Conditions for a ;evel, constant speed turn)\cr
  ..$ Cd_min: num 0.02(minimum Drag coefficient)\cr
  ..$ v     : num 20(translational speed of the turn)\cr
  ..$ H     : num 200(Height,not required if q is calculated)\cr
  ..$ k     : num 0.0398(lift induced drag factor)\cr
  ..$ n     : num 0.357(load factor)\cr
  ..$ q     : num 240(kinetic density)\cr
 $ ENERGY_LEVEL   :List of 8 (Conditions for a specific energy Level)\cr
  ..$ Cd_min: num 0.02(minimum Drag coefficient)\cr
  ..$ k     : num 0.0398(lift induced drag factor)\cr
  ..$ n     : num 0.181(load factor)\cr
  ..$ Ps    : num 1(Energy level, the extra power that is availiable at this state)\cr
  ..$ v     : num 20(speed)\cr
  ..$ H     : num 200(Height)\cr
  ..$ rho   : num 1.2(Density, can be calculated from H, is used to calculate q)\cr
  ..$ q     : num 240\cr
 $ CLIMB          :List of 7\cr
  ..$ v_v   : num 5(Rate of climb, vertical component of velocity)\cr
  ..$ v     : num 15(total velocity)\cr
  ..$ k     : num 0.0398\cr
  ..$ Cd_min: num 0.02\cr
  ..$ H     : num 200\cr
  ..$ q     : num 135\cr
  ..$ rho   : num 1.2\cr
 $ CRUISE_V       :List of 7\cr
  ..$ H     : num 200\cr
  ..$ k     : num 0.0398\cr
  ..$ Cd_min: num 0.02\cr
  ..$ v     : num 20(Cruise Speed)\cr
  ..$ H     : num 200\cr
  ..$ rho   : num 1.2\cr
  ..$ q     : num 240\cr
 $ SERVICE_CEILING:List of 6\cr
  ..$ v_v   : num 20\cr
  ..$ Cd_min: num 0.02\cr
  ..$ H     : num 400\cr
  ..$ k     : num 0.0398\cr
  ..$ rho   : num 1.18\cr
  ..$ q     : num 236\cr
 $ CL_MAX_STALL   :List of 1\cr
  ..$ q: num 50(kinetic density at the desired speed and height)\cr
 $ TO_DISTANCE    :List of 8\cr
  ..$ ClTO: num 1.2(Take-off Cl, usually when flap-on)\cr
  ..$ CdTO: num 0.5(Take-off Cd, usually when flap-on)\cr
  ..$ H   : num 0\cr
  ..$ v   : num 20\cr
  ..$ SG  : num 30(Ground running distance)\cr
  ..$ mew : num 0.2(Friction coefficient)\cr
  ..$ rho : num 1.22\cr
  ..$ q   : num 245\cr
 - attr(*, "class")= chr "Constraint"\cr
}
\details{
  The format is:\cr
List of 8\cr
 $ W_S            : num [1:451] 5 5.1 5.2 5.3 5.4 5.5 5.6 5.7 5.8 5.9 ...(List of W_S, wing loading to calculate)\cr
 $ TURN           :List of 6 (Conditions for a ;evel, constant speed turn)\cr
  ..$ Cd_min: num 0.02(minimum Drag coefficient)\cr
  ..$ v     : num 20(translational speed of the turn)\cr
  ..$ H     : num 200(Height,not required if q is calculated)\cr
  ..$ k     : num 0.0398(lift induced drag factor)\cr
  ..$ n     : num 0.357(load factor)\cr
  ..$ q     : num 240(kinetic density)\cr
 $ ENERGY_LEVEL   :List of 8 (Conditions for a specific energy Level)\cr
  ..$ Cd_min: num 0.02(minimum Drag coefficient)\cr
  ..$ k     : num 0.0398(lift induced drag factor)\cr
  ..$ n     : num 0.181(load factor)\cr
  ..$ Ps    : num 1(Energy level, the extra power that is availiable at this state)\cr
  ..$ v     : num 20(speed)\cr
  ..$ H     : num 200(Height)\cr
  ..$ rho   : num 1.2(Density, can be calculated from H, is used to calculate q)\cr
  ..$ q     : num 240\cr
 $ CLIMB          :List of 7\cr
  ..$ v_v   : num 5(Rate of climb, vertical component of velocity)\cr
  ..$ v     : num 15(total velocity)\cr
  ..$ k     : num 0.0398\cr
  ..$ Cd_min: num 0.02\cr
  ..$ H     : num 200\cr
  ..$ q     : num 135\cr
  ..$ rho   : num 1.2\cr
 $ CRUISE_V       :List of 7\cr
  ..$ H     : num 200\cr
  ..$ k     : num 0.0398\cr
  ..$ Cd_min: num 0.02\cr
  ..$ v     : num 20(Cruise Speed)\cr
  ..$ H     : num 200\cr
  ..$ rho   : num 1.2\cr
  ..$ q     : num 240\cr
 $ SERVICE_CEILING:List of 6\cr
  ..$ v_v   : num 20\cr
  ..$ Cd_min: num 0.02\cr
  ..$ H     : num 400\cr
  ..$ k     : num 0.0398\cr
  ..$ rho   : num 1.18\cr
  ..$ q     : num 236\cr
 $ CL_MAX_STALL   :List of 1\cr
  ..$ q: num 50(kinetic density at the desired speed and height)\cr
 $ TO_DISTANCE    :List of 8\cr
  ..$ ClTO: num 1.2(Take-off Cl, usually when flap-on)\cr
  ..$ CdTO: num 0.5(Take-off Cd, usually when flap-on)\cr
  ..$ H   : num 0\cr
  ..$ v   : num 20\cr
  ..$ SG  : num 30(Ground running distance)\cr
  ..$ mew : num 0.2(Friction coefficient)\cr
  ..$ rho : num 1.22\cr
  ..$ q   : num 245\cr
 - attr(*, "class")= chr "Constraint"\cr
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
create(Constraint.default)
## maybe str(Constraint.default) ; plot(Constraint.default) ...
}
\keyword{datasets}
