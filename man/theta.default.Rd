\name{theta.default}
\alias{theta.default}
\docType{data}
\title{
Default list to create an climb angle(Theta) analysis%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("theta.default")}
\format{
  The format is:
List of 3
 $ theta: num [1:651] 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 ...
 $ state:List of 6
  ..$ Cl : num 0.45
  ..$ Cd : num 0.02
  ..$ FG : num 15
  ..$ rho: num 1.21
  ..$ S  : num 0.7
  ..$ H  : num 100
 $ Pp   :List of 2
  ..$ F_max: num 7
  ..$ P_max: num 95
 - attr(*, "class")= chr "Theta"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Moxingfeijidetiaozhengyuanli by Tan Chu Xiong%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
create(theta.default)
## maybe str(theta.default) ; plot(theta.default) ...
}
\keyword{ObjectiveList}
