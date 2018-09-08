\name{create.Theta}
\alias{create.Theta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create analysis for climbing angle (Theta).
}
\description{
When a certain plane is climbng steadily at a certain angle there is the least speed, force and power to maintain  lift of the the corresponding state. In this model we assume that lift is fully used (the Cl and Cd entered is the Cl and Cd of the state).
Classified lists required for analysis. The output is also an objective list%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.Theta(Raw)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Raw}{
Raw objective list containing basic infomations for a kinetic study%%     ~~Describe \code{Raw} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Hao Li%%  ~~who you are~~
}
\note{
Inspired by the kinetic analysis from the book by Tan Chu Xiong%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{theta.default}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
create(theta.default)
## The function is currently defined as
function (Raw)
{
    re = list(Raw = Raw, Out = motion_LEAST_v_ANGLE(angle = Raw$theta,
        state = Raw$state))
    class(re) = "ThetaOut"
    re
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~Climb }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~Angle }% __ONLY ONE__ keyword per line
