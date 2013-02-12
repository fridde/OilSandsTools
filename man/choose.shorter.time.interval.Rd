\name{choose.shorter.time.interval}
\alias{choose.shorter.time.interval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
choose.shorter.time.interval(data.table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.table}{
%%     ~~Describe \code{data.table} here~~
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
function (data.table) 
{
    Project = standardize.name(production.data$Project, type = "Project")
    Company = standardize.name(production.data$Company, type = "Company")
    Data.Source.Quality = rep.int(0, nrow(data.table))
    Data.Source.Quality[data.table$Data.Source == "http://environment.alberta.ca/"] = 1
    time.intervals = sort(as.integer(unique(data.table$Time.Interval)))
    pick.away = c()
    for (i.time.intervals in time.intervals) {
        right.time.interval = which(data.table$Time.Interval == 
            paste(i.time.intervals))
        for (i.right.time.interval in right.time.interval) {
            same.project = which(Project == Project[i.right.time.interval])
            same.company = which(Company == Company[i.right.time.interval])
            larger.time.interval = which(as.integer(data.table$Time.Interval) > 
                i.time.intervals)
            worse.data.quality = which(Data.Source.Quality < 
                Data.Source.Quality[i.right.time.interval])
            same.date = which(data.table$Dates == data.table$Dates[i.right.time.interval])
            part1 = intersect(same.project, same.company)
            part2 = union(worse.data.quality, larger.time.interval)
            part2 = intersect(part2, same.date)
            pick.away = unique(c(pick.away, intersect(part1, 
                part2)))
        }
    }
    return.data.table = data.table
    if (length(pick.away) > 0) {
        return.data.table = data.table[-pick.away, ]
    }
    return(return.data.table)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
