\name{fill.from.above}
\alias{fill.from.above}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fill.from.above(data.vector, replace = NA, initial = length(data.vector))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.vector}{
%%     ~~Describe \code{data.vector} here~~
}
  \item{replace}{
%%     ~~Describe \code{replace} here~~
}
  \item{initial}{
%%     ~~Describe \code{initial} here~~
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
function (data.vector, replace = NA, initial = length(data.vector)) 
{
    if (length(data.vector) > 0) {
        if (!is.na(replace)) {
            data.vector[data.vector == replace] = NA
        }
        return.vector = data.vector
        if (is.na(return.vector[length(return.vector)])) {
            return.vector[length(return.vector)] = initial
        }
        for (ii in (length(return.vector) - 1):1) {
            if (is.na(return.vector[ii])) {
                return.vector[ii] = return.vector[ii + 1]
            }
        }
        return(return.vector)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
