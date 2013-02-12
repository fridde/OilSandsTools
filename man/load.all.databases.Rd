\name{load.all.databases}
\alias{load.all.databases}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
load.all.databases()
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
    rda.file.list = list.files(paste(main.dir, "data/", sep = ""), 
        pattern = ".rda")
    for (rda.file in rda.file.list) {
        load(paste(main.dir, "data/", rda.file, sep = ""), .GlobalEnv)
    }
    txt.file.list = list.files(paste(main.dir, "data/", sep = ""), 
        pattern = ".txt")
    for (txt.file in txt.file.list) {
        new.table = read.delim(paste(main.dir, "data/", txt.file, 
            sep = ""))
        table.name = gsub("_", ".", txt.file)
        table.name = gsub(".txt", "", table.name)
        assign(table.name, new.table, envir = .GlobalEnv)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
