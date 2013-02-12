\name{create.plot.collection}
\alias{create.plot.collection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.plot.collection()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    schemes = names(Compilation.schemes)
    schemes = schemes[(which(schemes == "KeepEmpty") + 1):length(schemes)]
    year.intervals = which(years.to.analize == "x", arr.ind = TRUE)
    for (ii in 1:length(year.intervals[, 1])) {
        start.year = years.to.analize$from.to[year.intervals[ii, 
            1]]
        end.year = gsub("X", "", names(years.to.analize)[year.intervals[ii, 
            2]])
        directory.name = paste("plots/", start.year, "_", end.year, 
            sep = "")
        cat(directory.name, " - ")
        dir.create(directory.name, showWarnings = FALSE)
        setwd(directory.name)
        current.dataset = time.filter(dataset, start.year, end.year)
        for (scheme in schemes) {
            filtered.dataset = filter.dataframe(current.dataset, 
                scheme)
            multi.line.plot(filtered.dataset, scheme)
        }
        setwd("../..")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
