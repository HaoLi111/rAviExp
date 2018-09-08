\name{create.Alpha_lin}
\alias{create.Alpha_lin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create lift and drag properties with thin airfoil, linear lift theory%%  ~~function to do ... ~~
}
\description{
Assuming Re is constan, using gradient of Cl with respect to angle of attack(AoA or alpha), zero-lift angle and lift-induced drag factor to generate lift and drag properties.
This would only be accurate for a small range of alpha. For more properties see 'Thin Airfoil Theory'.
}
\usage{
create.Alpha_lin(AAR, alpha = seq(from = -5, to = 10, by = 0.1))
}

\arguments{
  \item{AAR}{
  classified list of basic factors containing zero-lift angle, gradient of Cla, lift induced drag factor, zero-lift drag factor.

}
  \item{alpha}{
  Domain of angle of attack to loop for.
}
}
\references{
https://en.wikipedia.org/wiki/Airfoil

%% ~put references to the literature/web site here ~
}
\author{
Hao Li
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Alpha_lin.default}}

}
\examples{
create(Alpha_lin.default)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (AAR, alpha = seq(from = -5, to = 10, by = 0.1))
{
    Cl = AAR$Cla * (alpha - AAR$alpha0)
    Cd = AAR$Cd0 + Cl * Cl * AAR$CdiF
    k = Cl/Cd
    re = list(Raw = AAR, Out = data.frame(alpha = alpha, Cl = Cl,
        Cd = Cd, k = k))
    class(re) = "AlphaOut"
    re
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
