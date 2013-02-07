\name{synonym.names.second.part}
\alias{synonym.names.second.part}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an alias table. PART 2.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
synonym.names.second.part()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    setwd(main.dir)
    setwd("raw_data")
    syn.tab = read.table("synonym_names_raw_reconciled.txt", 
        sep = "\t", as.is = TRUE, header = TRUE)
    Main.Name = rbind(as.matrix(syn.tab$Main.Project.Name), as.matrix(syn.tab$Main.Company.Name))
    Alias.Name = rbind(as.matrix(syn.tab$Synonym.Project.Name), 
        as.matrix(syn.tab$Synonym.Company.Name))
    Type = rbind(as.matrix(syn.tab$Project.Type), as.matrix(syn.tab$Company.Type))
    synonym.names = data.frame(Main.Name, Alias.Name, Type, stringsAsFactors = FALSE, 
        row.names = NULL)
    synonym.names = synonym.names[synonym.names$Main.Name != 
        "", ]
    synonym.names = unique(synonym.names)
    synonym.names = synonym.names[order(synonym.names$Type, synonym.names$Main.Name), 
        ]
    setwd("..")
    setwd("converted_data")
    write.table(synonym.names, file = "synonym_names.txt", append = FALSE, 
        quote = TRUE, sep = ";", eol = "\n", na = "NA", dec = ".", 
        row.names = FALSE, col.names = TRUE)
    setwd("..")
    setwd("converted_data")
    production.data = read.table("production_data.txt", sep = ";", 
        as.is = TRUE, header = TRUE)
    project.plan = read.table("project_plan.txt", sep = ";", 
        as.is = TRUE, header = TRUE)
    synonym.names = read.table("synonym_names.txt", sep = ";", 
        as.is = TRUE, header = TRUE)
    empty.projects = which(production.data$Project == "")
    empty.companies = which(production.data$Company == "")
    if (length(c(empty.projects, empty.companies)) > 0) {
        for (proj.row in which(synonym.names$Type == "Project")) {
            matching.projects = grep(synonym.names$Alias.Name[proj.row], 
                production.data$Combined.Project.Name, ignore.case = TRUE)
            for (comp.row in which(synonym.names$Type == "Company")) {
                matching.companies = grep(synonym.names$Alias.Name[comp.row], 
                  production.data$Combined.Project.Name, ignore.case = TRUE)
                rows.to.change1 = intersect(empty.projects, empty.companies)
                rows.to.change2 = intersect(matching.projects, 
                  matching.companies)
                rows.to.change = intersect(rows.to.change1, rows.to.change2)
                if (length(rows.to.change) != 0) {
                  production.data$Project[rows.to.change] = synonym.names$Main.Name[proj.row]
                  production.data$Company[rows.to.change] = synonym.names$Main.Name[comp.row]
                }
            }
        }
        write.table(production.data, file = "production_data.txt", 
            append = FALSE, quote = TRUE, sep = ";", eol = "\n", 
            na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
    }
    Project = c(production.data$Project, project.plan$Project)
    Company = c(production.data$Company, project.plan$Company)
    Stage = c(rep.int("", length(production.data$Project)), project.plan$Stage)
    Main.Project = as.character(lapply(Project, standardize.name, 
        type = "Project"))
    Main.Company = as.character(lapply(Company, standardize.name, 
        type = "Company"))
    attribution.table = data.frame(Main.Project, Main.Company, 
        Project, Company, Stage, stringsAsFactors = FALSE)
    setwd("..")
    setwd("raw_data")
    write.table(attribution.table, file = "attributions_raw.txt", 
        append = FALSE, quote = TRUE, sep = ";", eol = "\n", 
        na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
    setwd("..")
    output.text = c("Now please follow the steps given in the chapter *Reconciling the attribute-table* in the Data COllection Manual")
    cat(output.text)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
