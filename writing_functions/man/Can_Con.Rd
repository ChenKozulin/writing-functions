\name{Can_Con}
\alias{Can_Con}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Can_Con(cleaf, CO2, RLWC, StoDen, Rnet, vdp, Tair)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cleaf}{
%%     ~~Describe \code{cleaf} here~~
}
  \item{CO2}{
%%     ~~Describe \code{CO2} here~~
}
  \item{RLWC}{
%%     ~~Describe \code{RLWC} here~~
}
  \item{StoDen}{
%%     ~~Describe \code{StoDen} here~~
}
  \item{Rnet}{
%%     ~~Describe \code{Rnet} here~~
}
  \item{vdp}{
%%     ~~Describe \code{vdp} here~~
}
  \item{Tair}{
%%     ~~Describe \code{Tair} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (cleaf, CO2, RLWC, StoDen, Rnet, vdp, Tair) 
{
    res = data.frame(cleaf, CO2, RLWC, StoDen, Rnet, vdp, Tair)
    CanCond = (Cleaf * StoDen) * CO2 * Rnet * vpd * Tair * RLWC
    cond = ifelse(Tair < 0, 0, cond)
    return(cond)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
