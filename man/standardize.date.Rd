\name{standardize.date}
\alias{standardize.date}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Change a given date into c.Date format.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
standardize.date(datestring, format = "auto")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datestring}{
%%     ~~Describe \code{datestring} here~~
}
  \item{format}{
%%     ~~Describe \code{format} here~~
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
function (datestring, format = "auto") 
{
    value = ""
    year = datestring[1]
    month = "06"
    day = "01"
    if (length(datestring) > 1) {
        month = datestring[2]
    }
    if (length(datestring) > 2) {
        day = datestring[3]
    }
    if (format == "auto") {
        if (grepl("Start", year)) {
            year = gsub("Start-up: ", "", year)
            value = as.Date(paste(year, month, day, sep = "-"))
        }
        if (grepl("00:00", year)) {
            value = as.Date(year)
        }
        if (length(datestring) == 2) {
            if (!is.na(month) && as.integer(month) <= 10) {
                month = paste("0", month, sep = "")
            }
            if (is.na(month)) {
                month = "06"
            }
            value = as.Date(paste(year, month, day, sep = "-"))
        }
    }
    return(value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
