\name{select.projects}
\alias{select.projects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Give the planned production rate for a given date.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
select.projects(date.value, prognosis.year, col.to.check = NA, literal = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date.value}{
%%     ~~Describe \code{date.value} here~~
}
  \item{prognosis.year}{
%%     ~~Describe \code{prognosis.year} here~~
}
  \item{col.to.check}{
%%     ~~Describe \code{col.to.check} here~~
}
  \item{literal}{
%%     ~~Describe \code{literal} here~~
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
function (date.value, prognosis.year, col.to.check = NA, literal = FALSE) 
{
    browser(text = "select.projects")
    setwd("converted_data")
    project.data = read.table("project_plan.txt", sep = ";", 
        dec = ".", as.is = TRUE, header = TRUE)
    setwd("..")
    for (ii in which(project.data$Type == "")) {
        project.data$Type[ii] = return.missing.Type(project.data$Project[ii], 
            project.data$Company[ii])
    }
    Exact.Type = project.data$Type
    project.data = cbind(project.data, Exact.Type)
    project.data$Type = lapply(project.data$Type, conglomerate.types)
    Values = numeric()
    for (ii in 1:length(project.data$Value)) {
        Values = c(Values, convert.to.barrel.per.day(project.data$Value[ii], 
            project.data$Unit.Def[ii]))
    }
    if (!literal) {
        project.data$Project = lapply(project.data$Project, standardize.name, 
            type = "Project")
        project.data$Company = lapply(project.data$Company, standardize.name, 
            type = "Company")
    }
    indices = 1:length(Values)
    if (!is.na(col.to.check)) {
        col.to.check = strsplit(col.to.check, ",")
        for (ii in 1:length(col.to.check[[1]])) {
            current.expression = strsplit(col.to.check[[1]][ii], 
                " ")[[1]]
            if (current.expression[2] == "==" || current.expression[2] == 
                "=") {
                matching.indices = which(project.data[current.expression[1]] == 
                  paste(current.expression[3:length(current.expression)], 
                    sep = " "))
                indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == "!=") {
                matching.indices = which(project.data[current.expression[1]] != 
                  paste(current.expression[3:length(current.expression)], 
                    sep = " "))
                indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == "<=") {
                matching.indices = which(project.data[current.expression[1]] <= 
                  as.numeric(paste(current.expression[3:length(current.expression)], 
                    sep = " ")))
                indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == ">=") {
                matching.indices = which(project.data[current.expression[1]] >= 
                  as.numeric(paste(current.expression[3:length(current.expression)], 
                    sep = " ")))
                indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == ">") {
                matching.indices = which(project.data[current.expression[1]] > 
                  as.numeric(paste(current.expression[3:length(current.expression)], 
                    sep = " ")))
                indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == "<") {
                matching.indices = which(project.data[current.expression[1]] < 
                  as.numeric(paste(current.expression[3:length(current.expression)], 
                    sep = " ")))
                indices = intersect(indices, matching.indices)
            }
        }
    }
    date.column = c.Date()
    for (ii in 1:length(project.data$Startup)) {
        date.column = c(date.column, standardize.date(c(project.data$Startup[ii])))
    }
    indices = intersect(indices, which(date.column == date.value))
    indices = intersect(indices, which(project.data$Year.of.Prognosis == 
        prognosis.year))
    return(sum(Values[indices]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
