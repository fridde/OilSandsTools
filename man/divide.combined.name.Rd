\name{divide.combined.name}
\alias{divide.combined.name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
divide.combined.name(Combined.Name.vector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Combined.Name.vector}{
%%     ~~Describe \code{Combined.Name.vector} here~~
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
function (Combined.Name.vector) 
{
    Project.vector = rep.int("", length(Combined.Name.vector))
    Company.vector = rep.int("", length(Combined.Name.vector))
    synonym.names.projects = synonym.names[synonym.names$Type == 
        "Project", ]
    synonym.names.companies = synonym.names[synonym.names$Type == 
        "Company", ]
    for (ii in 1:nrow(synonym.names.projects)) {
        i.project.found = grep(synonym.names.projects$Alias.Name[ii], 
            Combined.Name.vector)
        Project.vector[i.project.found] = synonym.names.projects$Alias.Name[ii]
    }
    for (ii in 1:nrow(synonym.names.projects)) {
        i.company.found = grep(synonym.names.companies$Alias.Name[ii], 
            Combined.Name.vector)
        Company.vector[i.company.found] = synonym.names.companies$Alias.Name[ii]
    }
    still.empty = union(which(Project.vector == ""), which(Company.vector == 
        ""))
    Combined.Names.to.seperate = Combined.Name.vector[still.empty]
    vector.to.fix = unique(grep(";", Combined.Names.to.seperate, 
        invert = TRUE, value = TRUE))
    fixed.vector = c()
    while (length(vector.to.fix) > 0) {
        vector.to.fix = fix(vector.to.fix)
        fixed.vector = c(fixed.vector, grep(";", vector.to.fix, 
            invert = FALSE, value = TRUE))
        vector.to.fix = grep(";", vector.to.fix, invert = TRUE, 
            value = TRUE)
    }
    for (i.fix in 1:length(fixed.vector)) {
        Combined.Names.to.seperate[Combined.Names.to.seperate == 
            gsub(";", "", fixed.vector[i.fix])] = fixed.vector[i.fix]
    }
    Combined.Name.vector[still.empty] = Combined.Names.to.seperate
    for (ii in still.empty) {
        Project.vector[ii] = strsplit(Combined.Name.vector[ii], 
            ";")[[1]][2]
        Company.vector[ii] = strsplit(Combined.Name.vector[ii], 
            ";")[[1]][1]
    }
    Project.vector = gsub("^[ \t]+|[ \t]+$", "", Project.vector)
    Company.vector = gsub("^[ \t]+|[ \t]+$", "", Company.vector)
    value = list(Project.vector, Company.vector)
    return(value)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
