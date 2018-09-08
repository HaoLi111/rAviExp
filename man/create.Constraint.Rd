\name{create.Constraint}
\alias{create.Constraint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a Constraint Analysis for T/W with respect to W/D from objective list
}
\description{
This model is from book General Aviation Aircraft by Snorri Gudmundsonn. Although 'constraint analysis' is a method applied in a lot of field in this package it is for Thrust to weight ratio (T/W or T/F_G) with respect to wing loading (F_G/S). Creation of this matrix work out the lower bound of Thrust to Weight ratio which suggests the boundary of the feasible range. Not all fields are required.
}
\usage{
create.Constraint(Cons = constraint.default, func = c("CA_TURN", "CA_ENERGY_LEVEL", "CA_CLIMB", "CA_CRUISE_V", "CA_SERVICE_CEILING", "CA_TO_DISTANCE"))
}
\arguments{
  \item{Cons}{
  The constraint list
}
  \item{func}{
  A string vector of functions to apply
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
Hao Li
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Constraint.default}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
create(Constraint.default)
## The function is currently defined as
function (Cons = constraint.default, func = c("CA_TURN", "CA_ENERGY_LEVEL",
    "CA_CLIMB", "CA_CRUISE_V", "CA_SERVICE_CEILING", "CA_TO_DISTANCE"))
{
    m <- matrix(rep(0, times = (length(func) + 1) * length(Cons$W_S)),
        ncol = length(func) + 1)
    m[, 1] <- Cons$W_S
    for (i in seq_along(func)) {
        m[, i + 1] <- get(func[i])(Cons)
    }
    colnames(m) = c("W_S", func)
    m = list(Constraint = Cons, Out = m)
    class(m) <- "ConstraintOut"
    m
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~Constraint }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~Analysis }
\keyword{ ~Feasible }
