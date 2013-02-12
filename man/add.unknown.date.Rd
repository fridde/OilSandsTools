\name{add.unknown.date}
\alias{add.unknown.date}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add.unknown.date(delay.value = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delay.value}{
%%     ~~Describe \code{delay.value} here~~
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
function (delay.value = 3) 
{
    project.plan = project.plan[order(project.plan$Project, project.plan$Company, 
        project.plan$Startup), ]
    for (ii in 1:nrow(project.plan)) {
        if (!(project.plan$has.Startup.date[ii])) {
            if (project.plan$Stage.Nr[ii] == "1") {
                new.date = as.POSIXlt(Sys.Date())
            }
            else {
                new.date = as.POSIXlt(project.plan$Startup[ii - 
                  1])
            }
            new.date$year = new.date$year + delay.value
            new.date = paste(format(new.date, format = "\%Y"), 
                "07-01", sep = "-", collapse = "")
            project.plan$Startup[ii] = new.date
        }
    }
    project.plan$Startup = as.Date(project.plan$Startup)
    save(project.plan, file = "data/project.plan.rda")
    return()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
