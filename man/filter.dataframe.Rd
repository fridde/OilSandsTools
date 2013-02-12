\name{filter.dataframe}
\alias{filter.dataframe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
filter.dataframe(dataframe, scheme)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{scheme}{
%%     ~~Describe \code{scheme} here~~
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
function (dataframe, scheme) 
{
    if (length(scheme) != 1) {
        paste("Too few, too many or non-existent schemes provided!")
    }
    columns.to.choose = Compilation.schemes$Scheme[which(Compilation.schemes[, 
        scheme] == "x")]
    years = Compilation.schemes$Year[which(Compilation.schemes[, 
        scheme] == "x")]
    columns.to.choose = columns.to.choose[order(years)]
    column.names = character()
    for (current.column in columns.to.choose) {
        current.short.name = Compilation.schemes$Short.Name[Compilation.schemes$Scheme == 
            current.column]
        if (current.short.name == "") {
            current.short.name = Compilation.schemes$Scheme[Compilation.schemes$Scheme == 
                current.column]
        }
        column.names = c(column.names, current.short.name)
    }
    return.dataframe = dataframe[c("Dates", columns.to.choose)]
    names(return.dataframe) = c("Dates", column.names)
    return(return.dataframe)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
