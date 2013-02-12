\name{update.attribution}
\alias{update.attribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
update.attribution(part = 1, new.projects.added = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{part}{
%%     ~~Describe \code{part} here~~
}
  \item{new.projects.added}{
%%     ~~Describe \code{new.projects.added} here~~
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
function (part = 1, new.projects.added = TRUE) 
{
    setwd(main.dir)
    if (part == 1 && new.projects.added) {
        new.attribution.table = attribution.table
        attribution.table.production = production.data[c("Project", 
            "Company", "Product", "Data.Source", "Data.Source.Year", 
            "Data.Type")]
        Data.Table.Type = rep.int("production.data", nrow(attribution.table.production))
        attribution.table.production = cbind(attribution.table.production, 
            Data.Table.Type)
        attribution.table.projects = project.plan[c("Project", 
            "Company", "Data.Source", "Data.Source.Year")]
        names(attribution.table.projects) = c("Project", "Company", 
            "Data.Source", "Data.Source.Year")
        n = nrow(attribution.table.projects)
        Product = rep.int("Bitumen", n)
        Data.Table.Type = rep.int("project.plan", n)
        Data.Type = rep.int("Prognosis", n)
        attribution.table.projects = cbind(attribution.table.projects, 
            Product, Data.Type, Data.Table.Type)
        new.attribution.table = rbind(attribution.table.production, 
            attribution.table.projects[, names(attribution.table.production)])
        new.attribution.table = unique(new.attribution.table)
        Main.Compilation = rep.int("", nrow(new.attribution.table))
        columns.to.include = names(attribution.table)[names(attribution.table) != 
            "Main.Compilation"]
        for (ii in 1:nrow(new.attribution.table)) {
            Main.Compilation[ii] = attribute.project(paste(new.attribution.table[ii, 
                columns.to.include]), columns.to.include)
            if (is.na(Main.Compilation[ii])) {
                Main.Compilation[ii] = paste(new.attribution.table[ii, 
                  ], sep = "", collapse = " - ")
            }
        }
        attributions.raw.table = cbind(Main.Compilation, new.attribution.table)
        attributions.raw.table = unique(attributions.raw.table)
        write.table(attributions.raw.table, file = "sources/attributions_raw.txt", 
            append = FALSE, quote = TRUE, sep = "\t", eol = "\n", 
            na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
    }
    if (part == 2) {
        if (new.projects.added) {
            attribution.table = read.table("sources/attributions_raw_reconciled.txt", 
                sep = "\t", as.is = TRUE, header = TRUE, quote = "\"")
            save(attribution.table, file = "data/attribution.table.rda")
        }
        quick.load()
        columns.to.include = c("Project", "Company", "Product", 
            "Data.Source", "Data.Source.Year", "Data.Type")
        if (is.null(production.data$Main.Compilation)) {
            Main.Compilation = rep.int("", nrow(production.data))
            production.data = cbind(Main.Compilation, production.data)
            save(production.data, file = "data/production.data.rda")
        }
        for (i.data.row in 1:nrow(production.data)) {
            production.data$Main.Compilation[i.data.row] = attribute.project(paste(production.data[i.data.row, 
                columns.to.include]), columns.to.include)
        }
        save(production.data, file = "data/production.data.rda")
        columns.to.include = c("Project", "Company", "Data.Source", 
            "Data.Source.Year")
        if (is.null(project.plan$Main.Compilation)) {
            Main.Compilation = rep.int("", nrow(project.plan))
            project.plan = cbind(Main.Compilation, project.plan)
            save(project.plan, file = "data/project.plan.rda")
        }
        for (i.data.row in 1:nrow(project.plan)) {
            project.plan$Main.Compilation[i.data.row] = attribute.project(paste(project.plan[i.data.row, 
                columns.to.include]), columns.to.include)
        }
        save(project.plan, file = "data/project.plan.rda")
    }
    quick.load()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
