\name{standardize.date.list}
\alias{standardize.date.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
standardize.date.list(datestring.matrix, format = "auto")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datestring.matrix}{
%%     ~~Describe \code{datestring.matrix} here~~
}
  \item{format}{
%%     ~~Describe \code{format} here~~
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
function (datestring.matrix, format = "auto") 
{
    datestring.matrix = as.matrix(datestring.matrix)
    if (length(format) == 1) {
        format = rep.int(format, nrow(datestring.matrix))
    }
    return.vector = c.Date()
    for (ii in 1:nrow(datestring.matrix)) {
        return.vector = c(return.vector, standardize.date(paste(datestring.matrix[ii, 
            ]), format = format[ii]))
    }
    return(return.vector)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
