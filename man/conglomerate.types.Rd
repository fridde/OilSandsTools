\name{conglomerate.types}
\alias{conglomerate.types}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Replaces the type of a project with a more common type to enable clustering.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
conglomerate.types(Type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Type}{
%%     ~~Describe \code{Type} here~~
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
function (Type) 
{
    browser(text = "conglomerate.types")
    alias.names.in.situ = c("SAGD", "TAGD", "ET-DSP", "In Situ", 
        "CSS", "SLP-SAGD", "SAP-SAGD", "Air injection", "SC-SAGD", 
        "Bitumen Extraction Solvent Techn", "LP-SAGD", "CSS & SAGD", 
        "THAI", "Vertical Steam Drive")
    alias.names.mining = c("Surface Mining")
    if (any(grepl(Type, alias.names.in.situ, ignore.case = TRUE))) {
        Type = "In Situ"
    }
    if (any(grepl(Type, alias.names.mining, ignore.case = TRUE))) {
        Type = "Mining"
    }
    return(Type)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
