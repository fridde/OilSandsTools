\name{interpolate.values}
\alias{interpolate.values}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
interpolate.values(dates, data.vector, type = "production.data")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dates}{
%%     ~~Describe \code{dates} here~~
}
  \item{data.vector}{
%%     ~~Describe \code{data.vector} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (dates, data.vector, type = "production.data") 
{
    datavector.original = data.vector
    n = length(datavector.original)
    if (type == "production.data") {
        vector.with.lower.indices = rep.int(NA, n)
        vector.with.lower.indices[!is.na(datavector.original)] = which(!is.na(datavector.original))
        vector.with.lower.indices = fill.from.below(vector.with.lower.indices)
        i.low = vector.with.lower.indices
        vector.with.higher.indices = rep.int(NA, n)
        vector.with.higher.indices[!is.na(datavector.original)] = which(!is.na(datavector.original))
        vector.with.higher.indices = fill.from.above(vector.with.higher.indices)
        i.high = vector.with.higher.indices
        for (ii in 1:n) {
            numerator = as.integer(dates[ii]) - as.integer(dates[i.low[ii]])
            denominator = as.integer(dates[i.high[ii]]) - as.integer(dates[i.low[ii]])
            if (denominator != 0) {
                date.fraction = numerator/denominator
            }
            else {
                date.fraction = 0
            }
            production.diff = datavector.original[i.high[ii]] - 
                datavector.original[i.low[ii]]
            data.vector[ii] = datavector.original[i.low[ii]] + 
                (production.diff * date.fraction)
        }
    }
    if (type == "project.plan") {
        for (ii in 1:n) {
            data.vector[ii] = sum(datavector.original[1:ii], 
                na.rm = TRUE)
        }
    }
    return(data.vector)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
