\name{Alpha_lin.default}
\alias{Alpha_lin.default}
\docType{data}
\title{
Default objective list to create an angle-of-attack analysis
}
\description{
classified list of basic factors containing zero-lift angle, gradient of Cla, lift induced drag factor, zero-lift drag factor.
}
\usage{data("Alpha_lin.default")}
\format{
  The format is:
List of 4
 $ Cla   : num 0.1
 $ alpha0: num -5
 $ CdiF  : num 0.0398
 $ Cd0   : num 0.02
 - attr(*, "class")= chr "Alpha_lin"
}
\details{
Cla: Rate of change of Cl with respect to alpha.
alpha0: Corresponded angle of attack when minimum absolute value of lift coefficient occurs
Cdif: lift-induced drag factor, also as the factor of the quadratic eq of the Cl versus alpha curve. Correction of the Cdif is usually related to the calculation of the Oswald Spanwise Efficiency.
Cd0: Minimum drag coefficient (Usually occurs when lift is close to 0).
}
\source{
}
\references{
https://en.wikipedia.org/wiki/Airfoil
}
\examples{
create(Alpha_lin.default)
## maybe str(Alpha_lin.default) ; plot(Alpha_lin.default) ...
}
\keyword{datasets}
