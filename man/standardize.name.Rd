\name{standardize.name}
\alias{standardize.name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a non-standard name for a project or company into its standardized name.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
standardize.name(alias.name, type = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alias.name}{
%%     ~~Describe \code{alias.name} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (alias.name, type = "") 
{
    if (!grepl("converted_data", getwd())) {
        setwd("converted_data")
    }
    synonym.names = read.table("synonym_names.txt", sep = ";", 
        as.is = TRUE, header = TRUE)
    setwd("..")
    if (type == "") {
        value = synonym.names$Main.Name[synonym.names$Alias.Name == 
            alias.name]
    }
    else {
        value1 = synonym.names$Main.Name[synonym.names$Alias.Name == 
            alias.name]
        value2 = synonym.names$Main.Name[synonym.names$Type == 
            type]
        value = intersect(value1, value2)
    }
    if (length(value) == 1) {
        return(value)
    }
    else {
        cat("Could not replace value", alias.name, ". Check synonym_names.txt. \n        Your value should exist exactly once in the column Alias.Name or a type has to be specified!")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
