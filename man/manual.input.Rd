\name{manual.input}
\alias{manual.input}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
manual.input(files.to.update)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files.to.update}{
%%     ~~Describe \code{files.to.update} here~~
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
function (files.to.update) 
{
    setwd("sources")
    files.to.update = paste(files.to.update, ".txt", sep = "")
    for (ii in 1:length(files.to.update)) {
        is.updated = FALSE
        while (!is.updated) {
            if (length(list.files(pattern = files.to.update[ii])) == 
                0) {
                write(c(), file = files.to.update[ii])
            }
            shell.exec(files.to.update[ii])
            is.updated = grepl("yes", varEntryDialog(vars = "Write Yes", 
                title = paste("Did you update and save", files.to.update[ii], 
                  "?"))[[1]], ignore.case = TRUE)
        }
    }
    setwd("..")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
