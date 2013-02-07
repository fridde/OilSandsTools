\name{return.missing.Type}
\alias{return.missing.Type}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fill in the type of a certain project.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
return.missing.Type(Project, Company)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Project}{
%%     ~~Describe \code{Project} here~~
}
  \item{Company}{
%%     ~~Describe \code{Company} here~~
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
function (Project, Company) 
{
    setwd("converted_data")
    project.data = read.table("project_plan.txt", sep = ";", 
        dec = ".", as.is = TRUE, header = TRUE)
    setwd("..")
    project.data$Project = lapply(project.data$Project, standardize.name, 
        type = "Project")
    project.data$Company = lapply(project.data$Company, standardize.name, 
        type = "Company")
    Type = project.data$Type[intersect(which(project.data$Project == 
        Project), which(project.data$Company == Company))]
    if (length(Type > 0)) {
        Type = names(table(Type))[table(Type) == max(table(Type))]
    }
    else {
        Type = ""
    }
    return(Type)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
