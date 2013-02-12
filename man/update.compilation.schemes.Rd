\name{update.compilation.schemes}
\alias{update.compilation.schemes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
update.compilation.schemes()
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
    new.values = setdiff(names(dataset), Compilation.schemes$Scheme)
    new.values = new.values[new.values != "Dates"]
    if (length(new.values) > 0) {
        new.rows = data.frame(matrix(data = "", nrow = length(new.values), 
            ncol = ncol(Compilation.schemes)))
        new.rows[, 1] = new.values
        names(new.rows) = names(Compilation.schemes)
        Compilation.schemes = rbind(Compilation.schemes, new.rows)
        write.table(Compilation.schemes, "data/Compilation_schemes.txt", 
            sep = "\t", na = "", quote = FALSE, row.names = FALSE)
        cat(paste("New values added into Compilation_schemes.txt: ", 
            new.values))
        quick.load()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
