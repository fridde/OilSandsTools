\name{SQLify}
\alias{SQLify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert files produced by user into neater format.
}
\description{
 Will convert a file into a "neater" format that follows a usable SQL-design and save this 
 into a folder "converted" beneath the current working directory

 The file should be produced by the steps given in http://goo.gl/tbQTf and contain its type in the filename or 
given into the function as the parameter source.type
Allowed types are: production_data_s_environment ,production_data_s_oilsandsreview, project_plan_2002_s_tdsecurities, 
project_plan_s_oilsandsreview
}
\usage{
SQLify(file.name, source.type = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
}
  \item{source.type}{
%%     ~~Describe \code{source.type} here~~
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
function (file.name, source.type = NA) 
{
    library("cwhmisc")
    library("gsubfn")
    setwd("raw_data")
    allowed.types = c("production_data_s_environment", "production_data_s_oilsandsreview", 
        "project_plan_2002_s_tdsecurities", "project_plan_s_oilsandsreview")
    source.type = grep(gsub(".txt", "", file.name), allowed.types)
    if (length(source.type) == 0) {
        source.type = "generic"
    }
    else {
        source.type = allowed.types[source.type]
    }
    if (source.type == "production_data_s_environment") {
        Data.Source.Name = "http://environment.alberta.ca/"
        data.table = read.table(file.name, sep = "\t", dec = ".", 
            as.is = TRUE, header = FALSE)
        names(data.table) = c("Combined.Project.Name", "Year", 
            "Value")
        Project = character(length = nrow(data.table))
        Company = character(length = nrow(data.table))
        Combined.Project.Name = data.table$Combined.Project.Name
        Year = data.table$Year
        Month = character(length = nrow(data.table))
        Value = data.table$Value
        Unit.Def = character(length = nrow(data.table))
        Data.Source = rep.int(Data.Source.Name, nrow(data.table))
        data.table = data.frame(Project, Company, Combined.Project.Name, 
            Year, Month, Value, Unit.Def, Data.Source, stringsAsFactors = FALSE)
        ii = which(data.table$Combined.Project.Name == "Project")
        data.table$Unit.Def[ii] = paste(data.table$Value[ii], 
            " (per year)")
        for (rownumber in 2:(nrow(data.table))) {
            if (data.table$Unit.Def[rownumber] == "") {
                data.table$Unit.Def[rownumber] = data.table$Unit.Def[rownumber - 
                  1]
            }
        }
        data.table = data.table[data.table$Combined.Project.Name != 
            "Project", ]
        setwd("..")
        setwd("converted_data")
        write.table(data.table, file = "production_data.txt", 
            append = FALSE, quote = TRUE, sep = ";", eol = "\n", 
            na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
        setwd("..")
    }
    if (source.type == "production_data_s_oilsandsreview") {
        Data.Source.Name = "http://www.oilsandsreview.com/"
        data.table = read.table(file.name, sep = "\t", dec = ".", 
            as.is = TRUE, header = FALSE, fill = TRUE)
        names(data.table) = c("Project.Name", "Company", "First.Year", 
            "empty1", "Second.Year", "empty2", "Difference")
        data.table.for.first.year = data.table[c("Project.Name", 
            "Company", "First.Year")]
        data.table.for.second.year = data.table[c("Project.Name", 
            "Company", "Second.Year")]
        names(data.table.for.first.year) = c("Project.Name", 
            "Company", "Value")
        names(data.table.for.second.year) = c("Project.Name", 
            "Company", "Value")
        data.table = rbind(data.table.for.first.year, data.table.for.second.year)
        Project = data.table$Project.Name
        Company = data.table$Company
        Combined.Project.Name = character(length = nrow(data.table))
        Year = character(length = nrow(data.table))
        Month = rep.int("6", nrow(data.table))
        Value = data.table$Value
        Unit.Def = rep.int("Barrels of Bitumen per day", nrow(data.table))
        Data.Source = rep.int(Data.Source.Name, nrow(data.table))
        data.table = data.frame(Project, Company, Combined.Project.Name, 
            Year, Month, Value, Unit.Def, Data.Source, stringsAsFactors = FALSE)
        ii = which(data.table$Project == "Project")
        data.table$Year[ii] = data.table$Value[ii]
        i.three = which(grepl("Three Months Ended", data.table$Project, 
            ignore.case = TRUE))
        i.six = which(grepl("Six Months Ended", data.table$Project, 
            ignore.case = TRUE))
        i.nine = which(grepl("Nine Months Ended", data.table$Project, 
            ignore.case = TRUE))
        i.twelve = which(grepl("Twelve Months Ended", data.table$Project, 
            ignore.case = TRUE))
        indices = list(i.three, i.six, i.nine, i.twelve)
        subtractor = c(1, 3, 4, 6)
        for (i.indices in 1:length(indices)) {
            for (i.months in 1:length(month.name)) {
                rows.matching.month = grep(month.name[i.months], 
                  data.table$Project, ignore.case = TRUE)
                rows.matching.month = intersect(rows.matching.month, 
                  indices[[i.indices]])
                data.table$Month[rows.matching.month] = (i.months + 
                  12 - subtractor[i.indices])\%\%12
            }
        }
        for (rownumber in 2:(nrow(data.table))) {
            if (data.table$Year[rownumber] == "") {
                data.table$Year[rownumber] = data.table$Year[rownumber - 
                  1]
            }
            if (data.table$Month[rownumber] == "") {
                data.table$Month[rownumber] = data.table$Month[rownumber - 
                  1]
            }
        }
        ij = which(!(data.table$Value == "" | data.table$Value == 
            "---" | data.table$Project == "Project" | data.table$Project == 
            "Total In Situ" | data.table$Project == "Total Mining" | 
            data.table$Project == "Grand Total"))
        data.table = data.table[ij, ]
        data.table = remove.dup.rows(data.table)
        setwd("..")
        setwd("converted_data")
        write.table(data.table, file = "production_data.txt", 
            append = TRUE, quote = TRUE, sep = ";", eol = "\n", 
            na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)
        setwd("..")
    }
    if (source.type == "project_plan_2002_s_tdsecurities") {
        Data.Source.Name = "http://www.imperialinvestmentrealty.com/TD\%20AlbertaOilSands.pdf"
        data.table = read.table(file.name, sep = ";", dec = ".", 
            as.is = TRUE, header = TRUE, fill = TRUE)
        names(data.table) = c("Plan.Id", "Project", "Company.ID", 
            "Company.Name", "Combined.Name", "Attributed.Project", 
            "Stage", "Costs", "Status", "AEUB.Status", "Startup", 
            "Production", "CumProduction", "Upgrader", "Description")
        Project = data.table$Project
        Company = data.table$Company.Name
        Stage = data.table$Stage
        Type = character(length = nrow(data.table))
        Costs = data.table$Costs
        Operating.Status = data.table$Status
        AEUB.Status = data.table$AEUB.Status
        Startup = data.table$Startup
        Value = data.table$Production
        Unit.Def = rep.int("Barrels of bitumen per day", nrow(data.table))
        Description = data.table$Description
        Descr.Updated = rep.int(gsub("[[:alpha:]_]", "", file.name), 
            nrow(data.table))
        Year.of.Prognosis = rep.int(gsub("[[:alpha:]_]", "", 
            file.name), nrow(data.table))
        Data.Source = rep.int(Data.Source.Name, nrow(data.table))
        data.table = data.frame(Project, Company, Stage, Type, 
            Costs, Operating.Status, AEUB.Status, Startup, Value, 
            Unit.Def, Description, Descr.Updated, Year.of.Prognosis, 
            Data.Source)
        setwd("..")
        setwd("converted_data")
        write.table(data.table, file = "project_plan.txt", append = FALSE, 
            quote = TRUE, sep = ";", eol = "\n", na = "NA", dec = ".", 
            row.names = FALSE, col.names = TRUE)
        setwd("..")
    }
    if (source.type == "project_plan_s_oilsandsreview") {
        Data.Source.Name = "http://www.oilsandsreview.com/statistics/projects.asp"
        write.table(gsub("['\"]", "", readLines(file(description = file.name))), 
            file = file.name, quote = FALSE, row.names = FALSE, 
            col.names = FALSE)
        data.table = read.table(file.name, sep = "\t", dec = ".", 
            as.is = TRUE, header = FALSE, fill = TRUE)
        names(data.table) = c("Company", "Project", "Phase", 
            "Updated", "Description", "Type", "Startup", "Value", 
            "Costs", "AEUB.Status")
        Project = data.table$Project
        Company = data.table$Company
        Stage = data.table$Phase
        Type = data.table$Type
        Costs = data.table$Costs
        Operating.Status = character(length = nrow(data.table))
        AEUB.Status = data.table$AEUB.Status
        Startup = data.table$Startup
        Value = data.table$Value
        Unit.Def = rep.int("Barrels of bitumen per day", nrow(data.table))
        Description = data.table$Description
        Descr.Updated = data.table$Updated
        Year.of.Prognosis = rep.int(paste(format(Sys.Date(), 
            "\%Y")), nrow(data.table))
        Data.Source = rep.int(Data.Source.Name, nrow(data.table))
        data.table = data.frame(Project, Company, Stage, Type, 
            Costs, Operating.Status, AEUB.Status, Startup, Value, 
            Unit.Def, Description, Descr.Updated, Year.of.Prognosis, 
            Data.Source)
        for (rownumber in 2:(nrow(data.table))) {
            if (data.table$Company[rownumber] == "") {
                data.table$Company[rownumber] = data.table$Company[rownumber - 
                  1]
            }
            if (data.table$Project[rownumber] == "") {
                data.table$Project[rownumber] = data.table$Project[rownumber - 
                  1]
            }
            if (data.table$Descr.Updated[rownumber] == "") {
                data.table$Descr.Updated[rownumber] = data.table$Descr.Updated[rownumber - 
                  1]
            }
        }
        for (rownumber in (nrow(data.table) - 1):1) {
            if (data.table$Description[rownumber] == "") {
                data.table$Description[rownumber] = data.table$Description[rownumber + 
                  1]
            }
        }
        data.table = data.table[which(data.table$AEUB.Status != 
            ""), ]
        data.table$Descr.Updated = gsub("Description:", "", data.table$Descr.Updated)
        data.table$Descr.Updated = gsub(":", "", data.table$Descr.Updated)
        data.table$Type = gsub("Type: ", "", data.table$Type)
        data.table$Startup = gsub("Startup: ", "", data.table$Startup)
        data.table$Value = gsub("bpd", "", data.table$Value)
        data.table$Value = gsub(",", "", data.table$Value)
        setwd("..")
        setwd("converted_data")
        write.table(data.table, file = "project_plan.txt", append = TRUE, 
            quote = TRUE, sep = ";", eol = "\n", na = "NA", dec = ".", 
            row.names = FALSE, col.names = FALSE)
        setwd("..")
    }
    return("Files converted. Find them in the subfolder /converted data/ .")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
