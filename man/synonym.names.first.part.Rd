\name{synonym.names.first.part}
\alias{synonym.names.first.part}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an alias table. Part 1.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
synonym.names.first.part(list.of.project.files.with.names, list.of.production.files.with.names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list.of.project.files.with.names}{
%%     ~~Describe \code{list.of.project.files.with.names} here~~
}
  \item{list.of.production.files.with.names}{
%%     ~~Describe \code{list.of.production.files.with.names} here~~
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
function (list.of.project.files.with.names, list.of.production.files.with.names) 
{
    proj.list = list.of.project.files.with.names
    prod.list = list.of.production.files.with.names
    setwd("converted_data")
    project.names = character()
    company.names = character()
    for (i.list in 1:length(proj.list)) {
        current.table = read.table(proj.list[[i.list]], sep = ";", 
            as.is = TRUE, header = TRUE)
        project.names = c(project.names, current.table$Project)
        company.names = c(company.names, current.table$Company)
    }
    for (i.list in 1:length(prod.list)) {
        current.table = read.table(prod.list[[i.list]], sep = ";", 
            as.is = TRUE, header = TRUE)
        current.table = current.table[which(current.table$Project != 
            ""), ]
        project.names = c(project.names, current.table$Project)
        company.names = c(company.names, current.table$Company)
    }
    Project.Name = sort(unique(project.names))
    Company.Name = sort(unique(company.names))
    Company.Name = c(Company.Name, rep.int("", length(Project.Name) - 
        length(Company.Name)))
    Project.Type = rep.int("Project", length(Project.Name))
    Company.Type = rep.int("Company", length(Company.Name))
    Company.Type = c(Company.Type, rep.int("", length(Project.Type) - 
        length(Company.Type)))
    synonym.table = data.frame(Main.Project.Name = Project.Name, 
        Synonym.Project.Name = Project.Name, Project.Type, Main.Company.Name = Company.Name, 
        Synonym.Company.Name = Company.Name, Company.Type)
    setwd("..")
    setwd("raw_data")
    write.table(synonym.table, file = "synonym_names_raw.txt", 
        append = FALSE, quote = TRUE, sep = ";", eol = "\n", 
        na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
    setwd("..")
    setwd("converted_data")
    empty.fields.exist = logical()
    for (i.list in 1:length(prod.list)) {
        current.table = read.table(prod.list[[i.list]], sep = ";", 
            as.is = TRUE, header = TRUE)
        empty.fields.exist = c(empty.fields.exist, any(current.table$Project == 
            ""))
        empty.fields.exist = c(empty.fields.exist, any(current.table$Company == 
            ""))
    }
    if (any(empty.fields.exist)) {
        comb.project.name = character()
        company.name = character()
        project.name = character()
        for (i.list in 1:length(prod.list)) {
            current.table = read.table(prod.list[[i.list]], sep = ";", 
                as.is = TRUE, header = TRUE)
            comb.project.name = c(comb.project.name, current.table$Combined.Project.Name)
            company.name = c(company.name, current.table$Company)
            project.name = c(project.name, current.table$Project)
        }
        prod.table = data.frame(project.name, company.name, comb.project.name, 
            stringsAsFactors = FALSE)
        prod.table = prod.table[which(prod.table$project.name == 
            ""), ]
        for (i.syn.proj.name in 1:length(synonym.table$Synonym.Project.Name)) {
            ii = grep(paste(synonym.table$Synonym.Project.Name[i.syn.proj.name]), 
                prod.table$comb.project.name)
            prod.table$project.name[ii] = paste(synonym.table$Synonym.Project.Name[i.syn.proj.name])
        }
        for (i.syn.comp.name in 1:length(synonym.table$Synonym.Company.Name)) {
            ii = grep(paste(synonym.table$Synonym.Company.Name[i.syn.comp.name]), 
                prod.table$comb.project.name)
            prod.table$company.name[ii] = paste(synonym.table$Synonym.Company.Name[i.syn.comp.name])
        }
        ij = intersect(which(prod.table$company.name == ""), 
            which(prod.table$project.name != ""))
        for (k in ij) {
            prod.table$company.name[k] = gsub(prod.table$project.name[k], 
                "", prod.table$comb.project.name[k])
        }
        ji = intersect(which(prod.table$project.name == ""), 
            which(prod.table$company.name != ""))
        for (k in ji) {
            prod.table$project.name[k] = gsub(prod.table$company.name[k], 
                "", prod.table$comb.project.name[k])
        }
        ijk = which(prod.table$project.name != "")
        for (i.ijk in ijk) {
            prod.table$comb.project.name[i.ijk] = sub(prod.table$project.name[i.ijk], 
                paste(";", prod.table$project.name[i.ijk], sep = ""), 
                prod.table$comb.project.name[i.ijk])
        }
        iij = grep("^;", prod.table$comb.project.name)
        prod.table$comb.project.name[iij] = gsub(";", "", prod.table$comb.project.name[iij])
        while (!(all(grepl(";", prod.table$comb.project.name)))) {
            vector.to.fix = prod.table$comb.project.name[grep(";", 
                prod.table$comb.project.name, invert = TRUE)]
            vector.to.fix = fix(vector.to.fix)
            fixed.vector = grep(";", vector.to.fix, value = TRUE)
            for (i.fix in 1:length(fixed.vector)) {
                vector.to.fix[vector.to.fix == gsub(";", "", 
                  fixed.vector[i.fix])] = fixed.vector[i.fix]
            }
            prod.table$comb.project.name[grep(";", prod.table$comb.project.name, 
                invert = TRUE)] = vector.to.fix
        }
        for (i.splitted in 1:length(prod.table$comb.project.name)) {
            prod.table$project.name[i.splitted] = strsplit(prod.table$comb.project.name[i.splitted], 
                ";")[[1]][2]
            prod.table$company.name[i.splitted] = strsplit(prod.table$comb.project.name[i.splitted], 
                ";")[[1]][1]
        }
    }
    if (exists("prod.table")) {
        Main.Project.Name = prod.table$project.name
        Synonym.Project.Name = prod.table$project.name
        Project.Type = rep.int("Project", length(Main.Project.Name))
        Main.Company.Name = prod.table$company.name
        Synonym.Company.Name = prod.table$company.name
        Company.Type = rep.int("Company", length(Main.Company.Name))
        proj.synonym.table = data.frame(Main.Project.Name, Synonym.Project.Name, 
            Project.Type, stringsAsFactors = FALSE, row.names = NULL)
        comp.synonym.table = data.frame(Main.Company.Name, Synonym.Company.Name, 
            Company.Type, stringsAsFactors = FALSE, row.names = NULL)
        for (ijk in 1:ncol(proj.synonym.table)) {
            proj.synonym.table[[ijk]] = gsub("^[ \t]+|[ \t]+$", 
                "", proj.synonym.table[[ijk]])
            comp.synonym.table[[ijk]] = gsub("^[ \t]+|[ \t]+$", 
                "", comp.synonym.table[[ijk]])
        }
        proj.synonym.table = unique(proj.synonym.table, row.names = NULL)
        comp.synonym.table = unique(comp.synonym.table, row.names = NULL)
        if (nrow(proj.synonym.table) > nrow(comp.synonym.table)) {
            comp.synonym.table = rbind(as.matrix(comp.synonym.table), 
                matrix("", nrow = (nrow(proj.synonym.table) - 
                  nrow(comp.synonym.table)), ncol = ncol(comp.synonym.table)))
        }
        if (nrow(proj.synonym.table) < nrow(comp.synonym.table)) {
            proj.synonym.table = rbind(as.matrix(proj.synonym.table), 
                matrix("", nrow = (nrow(comp.synonym.table) - 
                  nrow(proj.synonym.table)), ncol = ncol(proj.synonym.table)))
        }
        synonym.table = data.frame(proj.synonym.table, comp.synonym.table, 
            stringsAsFactors = FALSE, row.names = NULL)
        setwd("..")
        setwd("raw_data")
        write.table(synonym.table, file = "synonym_names_raw.txt", 
            append = TRUE, quote = TRUE, sep = ";", eol = "\n", 
            na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)
        setwd("..")
    }
    output.text = c("Now please follow the steps given in the chapter *Reconciling the synonym table* in the Data COllection Manual")
    cat(output.text)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
