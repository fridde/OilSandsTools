\name{extract.all.dates}
\alias{extract.all.dates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract all dates available in the data.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract.all.dates()
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
    setwd("converted_data")
    project.plan = read.table("project_plan.txt", sep = ";", 
        dec = ".", as.is = TRUE, header = TRUE)
    production.data = read.table("production_data.txt", sep = ";", 
        dec = ".", as.is = TRUE, header = TRUE)
    setwd("..")
    production.dates = c.Date()
    for (ii in 1:length(production.data$Year)) {
        production.dates = c(production.dates, standardize.date(c(production.data$Year[ii], 
            production.data$Month[ii])))
    }
    project.dates = c.Date()
    for (ij in 1:length(project.plan$Startup)) {
        project.dates = c(project.dates, standardize.date(project.plan$Startup[ij]))
    }
    value = as.Date(c(project.dates, production.dates))
    value = sort(unique(value))
    return(value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
