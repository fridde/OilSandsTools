\name{attribute.project}
\alias{attribute.project}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
attribute.project(project.values.string, columns.to.include = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{project.values.string}{
%%     ~~Describe \code{project.values.string} here~~
}
  \item{columns.to.include}{
%%     ~~Describe \code{columns.to.include} here~~
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
function (project.values.string, columns.to.include = "") 
{
    if (length(columns.to.include) == 1 && columns.to.include == 
        "") {
        columns.to.include = names(attribution.table)
        columns.to.include = columns.to.include[columns.to.include != 
            "Main.Compilation"]
    }
    values.to.test = rep.int(FALSE, nrow(attribution.table))
    for (ii in 1:nrow(attribution.table)) {
        values.to.test[ii] = all(project.values.string == paste(attribution.table[ii, 
            columns.to.include]))
    }
    if (length(which(values.to.test)) == 1) {
        Main.Compilation = attribution.table$Main.Compilation[which(values.to.test)]
    }
    else {
        Main.Compilation = NA
    }
    return(Main.Compilation)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
