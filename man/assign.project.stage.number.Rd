\name{assign.project.stage.number}
\alias{assign.project.stage.number}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
assign.project.stage.number(string.to.match)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string.to.match}{
%%     ~~Describe \code{string.to.match} here~~
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
function (string.to.match) 
{
    usable.columns = names(stage.numbers)[names(stage.numbers) != 
        "Stage.Nr"]
    string.to.match[string.to.match == "NA"] = ""
    matching.rows = numeric()
    for (ii in 1:nrow(stage.numbers)) {
        if (all(string.to.match == paste(stage.numbers[ii, usable.columns]))) {
            matching.rows = c(matching.rows, ii)
        }
    }
    if (length(matching.rows) == 0) {
        stage.number = ""
    }
    if (length(matching.rows) == 1) {
        stage.number = stage.numbers$Stage.Nr[matching.rows]
    }
    if (length(matching.rows) > 1) {
        stage.number = "Too many possibilities. Check your algorithm!"
    }
    return(stage.number)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
