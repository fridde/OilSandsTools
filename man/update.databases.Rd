\name{update.databases}
\alias{update.databases}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
update.databases(with.manual.input = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{with.manual.input}{
%%     ~~Describe \code{with.manual.input} here~~
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
function (with.manual.input = FALSE) 
{
    rm(list = ls())
    main.dir = "C:/Users/Friedrich.Hehl/Dropbox/exjobbet/calculations/OilSandsTools/"
    setwd(main.dir)
    source("R/quick.load.r")
    quick.load()
    production.data = production.data[is.na(production.data$Dates), 
        ]
    save(production.data, file = "data/production.data.rda")
    project.plan = project.plan[is.na(project.plan$Main.Compilation), 
        ]
    save(project.plan, file = "data/project.plan.rda")
    if (exists("with.manual.input") && with.manual.input) {
        files.to.update = list.files(path = "sources/", pattern = "updateable\\.txt")
        manual.input(files.to.update)
    }
    files.to.update = list.files(path = "sources/", pattern = ".txt")
    for (ii in 1:length(files.to.update)) {
        file.name = paste(files.to.update[ii])
        SQLify(file.name)
    }
    update.attribution(part = 1)
    update.attribution(part = 2)
    update.project.stage(part = 1)
    update.project.stage(part = 2)
    quick.load()
    all.the.dates = extract.all.dates()
    save(all.the.dates, file = "data/all.the.dates.rda")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
