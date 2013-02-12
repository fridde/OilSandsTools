\name{extract.sample.coefficients}
\alias{extract.sample.coefficients}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract.sample.coefficients(data.set)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.set}{
%%     ~~Describe \code{data.set} here~~
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
function (data.set) 
{
    prog.columns = grep("Plan", names(data.set), ignore.case = TRUE)
    nr.prog.columns = length(prog.columns)
    for (i.prog.col in prog.columns) {
        quota = data.set$Production/data.set[i.prog.col]
        data.set = cbind(data.set, quota)
        names(data.set)[ncol(data.set)] = paste("Quota", names(data.set[i.prog.col]), 
            sep = ".")
    }
    return.data.set = data.set
    return(return.data.set)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
