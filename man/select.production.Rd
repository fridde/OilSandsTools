\name{select.production}
\alias{select.production}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Give the production rate of a given date.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
select.production(date.value, col.to.check = NA, literal = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date.value}{
%%     ~~Describe \code{date.value} here~~
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
function (date.value, col.to.check = NA, literal = FALSE) 
{
    browser(text = "select.production")
    setwd("converted_data")
    production.data = read.table("production_data.txt", sep = ";", 
        dec = ".", as.is = TRUE, header = TRUE)
    setwd("..")
    Values = numeric()
    for (ii in 1:length(production.data$Value)) {
        Values = c(Values, convert.to.barrel.per.day(production.data$Value[ii], 
            production.data$Unit.Def[ii]))
    }
    if (!literal) {
        for (ii in 1:length(production.data$Project)) {
            production.data$Project[ii] = standardize.name(production.data$Project[ii], 
                type = "Project")
            production.data$Company[ii] = standardize.name(production.data$Company[ii], 
                type = "Company")
        }
    }
    Exact.Type = c()
    for (ii in 1:nrow(production.data)) {
        Exact.Type = c(Exact.Type, return.missing.Type(production.data$Project[ii], 
            production.data$Company[ii]))
    }
    Type = Exact.Type
    production.data = cbind(production.data, Type, Exact.Type)
    production.data$Type = lapply(production.data$Type, conglomerate.types)
    indices = 1:length(Values)
    if (!is.na(col.to.check)) {
        col.to.check = strsplit(col.to.check, ",")
        for (ii in 1:length(col.to.check[[1]])) {
            current.expression = strsplit(col.to.check[[1]][ii], 
                " ")[[1]]
            if (current.expression[2] == "==" || current.expression[2] == 
                "=") {
                matching.indices = which(production.data[current.expression[1]] == 
                  paste(current.expression[3:length(current.expression)], 
                    sep = " "))
                indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == "!=") {
                matching.indices = which(production.data[current.expression[1]] != 
                  paste(current.expression[3:length(current.expression)], 
                    sep = " "))
                indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == "<=") {
                matching.indices = which(production.data[current.expression[1]] <= 
                  as.numeric(paste(current.expression[3:length(current.expression)], 
                    sep = " ")))
                indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == ">=") {
                matching.indices = which(production.data[current.expression[1]] >= 
                  as.numeric(paste(current.expression[3:length(current.expression)], 
                    sep = " ")))
                indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == ">") {
                matching.indices = which(production.data[current.expression[1]] > 
                  as.numeric(paste(current.expression[3:length(current.expression)], 
                    sep = " ")))
                indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == "<") {
                matching.indices = which(production.data[current.expression[1]] < 
                  as.numeric(paste(current.expression[3:length(current.expression)], 
                    sep = " ")))
                indices = intersect(indices, matching.indices)
            }
        }
    }
    date.column = c.Date()
    for (ii in 1:length(production.data$Year)) {
        date.column = c(date.column, standardize.date(c(production.data$Year[ii], 
            production.data$Month[ii])))
    }
    indices = intersect(indices, which(date.column == date.value))
    return(sum(Values[indices]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
