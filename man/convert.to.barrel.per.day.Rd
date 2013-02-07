\name{convert.to.barrel.per.day}
\alias{convert.to.barrel.per.day}

\title{
Convert to barrels per day.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convert.to.barrel.per.day(value, unit.def = "Barrels of Bitumen per day")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value}{
%%     ~~Describe \code{value} here~~
}
  \item{unit.def}{
%%     ~~Describe \code{unit.def} here~~
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
function (value, unit.def = "Barrels of Bitumen per day") 
{
    value = as.numeric(gsub(",", "", value))
    if (unit.def == "Production (m3) - Bitumen  (per year)") {
        value = value * (1/365) * (1/0.158987294928)
    }
    if (unit.def == "Production (m3) - Synthetic Crude Oil  (per year)") {
        value = value * (1/0.158987294928) * (1/365) * (1/0.86)
    }
    if (unit.def == "Barrels of Bitumen per day") {
        value = value
    }
    value = round(value)
    return(value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
