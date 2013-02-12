\name{select.historic}
\alias{select.historic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
select.historic(date.value.vector, col.to.check = NA, compilation = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date.value.vector}{
%%     ~~Describe \code{date.value.vector} here~~
}
  \item{col.to.check}{
%%     ~~Describe \code{col.to.check} here~~
}
  \item{compilation}{
%%     ~~Describe \code{compilation} here~~
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
function (date.value.vector, col.to.check = NA, compilation = "") 
{
    data.table.type = attribution.table$Data.Table.Type[min(which(attribution.table$Main.Compilation == 
        compilation), na.rm = TRUE)]
    if (!is.na(col.to.check)) {
        col.to.check = strsplit(col.to.check, ",")[[1]]
    }
    compilation.indices = which(production.data$Main.Compilation == 
        compilation)
    production.values.vector = c()
    for (i.date in 1:length(date.value.vector)) {
        indices = compilation.indices
        if (!is.na(col.to.check)) {
            for (ii in 1:length(col.to.check)) {
                current.expression = strsplit(col.to.check[ii], 
                  " ")[[1]]
                current.expression = gsub("^[ \t]+|[ \t]+$", 
                  "", current.expression)
                if (current.expression[2] == "==" | current.expression[2] == 
                  "=") {
                  matching.indices = which(production.data[current.expression[1]] == 
                    paste(current.expression[3:length(current.expression)], 
                      collapse = " "))
                  indices = intersect(indices, matching.indices)
                }
                if (current.expression[2] == "!=") {
                  matching.indices = which(production.data[current.expression[1]] != 
                    paste(current.expression[3:length(current.expression)], 
                      collapse = " "))
                  indices = intersect(indices, matching.indices)
                }
                if (current.expression[2] == "<=") {
                  matching.indices = which(production.data[current.expression[1]] <= 
                    as.numeric(paste(current.expression[3:length(current.expression)], 
                      collapse = " ")))
                  indices = intersect(indices, matching.indices)
                }
                if (current.expression[2] == ">=") {
                  matching.indices = which(production.data[current.expression[1]] >= 
                    as.numeric(paste(current.expression[3:length(current.expression)], 
                      collapse = " ")))
                  indices = intersect(indices, matching.indices)
                }
                if (current.expression[2] == ">") {
                  matching.indices = which(production.data[current.expression[1]] > 
                    as.numeric(paste(current.expression[3:length(current.expression)], 
                      collapse = " ")))
                  indices = intersect(indices, matching.indices)
                }
                if (current.expression[2] == "<") {
                  matching.indices = which(production.data[current.expression[1]] < 
                    as.numeric(paste(current.expression[3:length(current.expression)], 
                      collapse = " ")))
                  indices = intersect(indices, matching.indices)
                }
            }
        }
        indices = intersect(indices, which(production.data$Dates == 
            date.value.vector[i.date]))
        indices = intersect(indices, compilation.indices)
        Values = convert.to.barrel.per.day(production.data$Value[indices], 
            production.data$Unit.Def[indices])
        production.values.vector[i.date] = sum(Values, na.rm = TRUE)
    }
    production.values.vector[production.values.vector == "0"] = NA
    production.values.vector = interpolate.values(date.value.vector, 
        production.values.vector, type = data.table.type)
    return(production.values.vector)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
