\name{compile.prod.proj}
\alias{compile.prod.proj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a data frame with production and prognosis for a given date vector.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compile.prod.proj(col.to.check = NA, start.year = "1900", end.year = "2499", prognosis.years = "2002, 2012")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col.to.check}{
%%     ~~Describe \code{col.to.check} here~~
}
  \item{start.year}{
%%     ~~Describe \code{start.year} here~~
}
  \item{end.year}{
%%     ~~Describe \code{end.year} here~~
}
  \item{prognosis.years}{
%%     ~~Describe \code{prognosis.years} here~~
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
function (col.to.check = NA, start.year = "1900", end.year = "2499", 
    prognosis.years = "2002, 2012") 
{
    date.column = time.filter(extract.all.dates(), start.year, 
        end.year)
    prognosis.years = as.integer(strsplit(prognosis.years, ",")[[1]])
    prod.values = lapply(date.column, select.production, col.to.check)
    prog.values = matrix(nrow = length(date.column), ncol = length(prognosis.years))
    for (i.year in 1:length(prognosis.years)) {
        for (i.date in 1:length(date.column)) {
            prog.values[i.date, i.year] = select.projects(date.column[i.date], 
                prognosis.years[i.years], col.to.check)
        }
    }
    value = data.frame(date.column, Production = prod.values, 
        prog.values[, 1:ncol(prog.values)])
    names(value) = c("Date", "Production", paste("Plan", prognosis.years, 
        sep = "."))
    return(value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
