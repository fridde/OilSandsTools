\name{time.filter}
\alias{time.filter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Filters a given vector of c.Date values so that it only contains a certain time range.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
time.filter(date.vector, start.year = "1900", end.year = "2499", start.day = "1900-01-01", end.day = "2500-01-01")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date.vector}{
%%     ~~Describe \code{date.vector} here~~
}
  \item{start.year}{
%%     ~~Describe \code{start.year} here~~
}
  \item{end.year}{
%%     ~~Describe \code{end.year} here~~
}
  \item{start.day}{
%%     ~~Describe \code{start.day} here~~
}
  \item{end.day}{
%%     ~~Describe \code{end.day} here~~
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
function (date.vector, start.year = "1900", end.year = "2499", 
    start.day = "1900-01-01", end.day = "2500-01-01") 
{
    if (start.year != "1900") {
        start.day = paste(start.year, "-01-01", sep = "")
    }
    if (end.year != "2499") {
        end.day = paste(end.year, "-12-31", sep = "")
    }
    date.vector = as.Date(date.vector)
    start.day = as.Date(start.day)
    end.day = as.Date(end.day)
    value = date.vector[intersect(which(date.vector <= end.day), 
        which(date.vector >= start.day))]
    return(value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
