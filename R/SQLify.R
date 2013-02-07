SQLify <- function (file.name) {
  
  # dt means data.table
  quick.load()
  setwd(main.dir)
  setwd("sources")
  allowed.types = c("production_data_s_environment_updateable" ,"production_data_s_oilsandsreview_updateable", 
                    "project_plan_2002_s_tdsecurities", "project_plan_s_oilsandsreview_updateable", 
                    "capp_forecast_2006", "capp_forecast_2012", "capp_historical_2012", 
                    "projection_aeub_2005", "projection_neb_2003", "projection_neb_2007", "projection_neb_2009",
                    "projection_ostr_2004", "citibank_forecast_2012", "canada_stat_hist_2012", "projection_neb_2000",
                    "CERI_2003", "CERI_2011", "CERI_2011_status", "projection_neb_2011", "WEO_2012")
  source.type = grep(file.name, paste(allowed.types, ".txt", sep = ""))
  if (length(source.type) == 0)  {
    source.type = "generic"
  } else {
    source.type = allowed.types[source.type]
  }
  
  cat(paste(" +++ Updating", source.type))
  
  
  if (source.type=="production_data_s_environment_updateable") {
    
    Data.Source.Name = "http://environment.alberta.ca/"
    
    dt = read.table(file.name, sep= "\n", blank.lines.skip = FALSE)
    Year.row = grep("Year of last change", dt[,1])
    Data.Source.Year = gsub("Year of last change: ", "", dt[Year.row,1])
    
    dt.names = c("Combined.Project.Name", "Year", "Value")
    dt = read.table(file.name, sep= "\t", dec =".", as.is = TRUE, header = FALSE, col.names = dt.names, skip = Year.row) 
    
    if (nrow(dt) > 0) {
      
      Project = character(length = nrow(dt))
      Company = character(length = nrow(dt))
      Combined.Project.Name = dt$Combined.Project.Name
      Year = dt$Year
      Month = character(length = nrow(dt))
      Value = dt$Value
      Unit.Def = character(length = nrow(dt))
      Data.Source = rep.int(Data.Source.Name, nrow(dt))
      Product = character(length = nrow(dt))
      Time.Interval = rep.int("12", nrow(dt))
      Data.Source.Year = rep.int(Data.Source.Year, nrow(dt))
      Data.Type = rep.int("Historical", nrow(dt))
      
      dt = data.frame(Project, Company, Combined.Project.Name, Year, Month, Value, Unit.Def, Product,
                              Data.Source, Time.Interval, Data.Source.Year, Data.Type)
      
      dt$Unit.Def[dt$Combined.Project.Name == "Project"] =
        paste(dt$Value[dt$Combined.Project.Name == "Project"], " (per year)", sep = "")
      
      dt$Unit.Def[dt$Unit.Def == ""] = NA
      dt$Unit.Def = fill.from.below(dt$Unit.Def)
      dt$Product[dt$Unit.Def == "Production (m3) - Synthetic Crude Oil  (per year)"] = "SCO"
      dt$Product[dt$Unit.Def == "Production (m3) - Bitumen  (per year)"] = "Bitumen"
      
      dt = dt[dt$Combined.Project.Name != "Project",] # erase all rows that were headers once
      
      # now fill in the the missing projects and/or companies with the help of Combined.Project.Name
      empty.cells.indices = which(dt$Project == "" | dt$Company == "")
      Project.Company.list = divide.combined.name(dt$Combined.Project.Name[empty.cells.indices])
      dt$Project[empty.cells.indices] = Project.Company.list[[1]]
      dt$Project[dt$Project == "Syncrude"] = "Syncrude Upgrader" # replace misnomers
      dt$Project[dt$Project == "Suncor"] = "Base Operations"       # replace misnomers
      
      dt$Company[empty.cells.indices] = Project.Company.list[[2]]
      Dates = standardize.date.list(cbind(dt$Year, dt$Month))
      dt = cbind(dt, Dates)
    }
    
    dt.new = dt[production.data.names]
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")    
    
  }
  
  if (source.type=="production_data_s_oilsandsreview_updateable") {
    
    Data.Source.Name = "http://www.oilsandsreview.com/"
    
    dt = read.table(file.name, sep= "\n", blank.lines.skip = FALSE)
    Year.row = grep("Year of last change", dt[,1])
    Data.Source.Year = gsub("Year of last change: ", "", dt[Year.row,1])
    
    dt.names = c("Project.Name", "Company", "First.Year", "empty1", "Second.Year", "empty2", "Difference")
    dt = read.table(file.name, sep= "\t", dec =".", as.is = TRUE, skip = Year.row,
                    header = FALSE, fill = TRUE, col.names = dt.names)
    
    if (nrow(dt) > 1) {
      
      dt.for.first.year = dt[c("Project.Name", "Company", "First.Year")]
      dt.for.second.year = dt[c("Project.Name", "Company", "Second.Year")]
      names(dt.for.first.year) = c("Project.Name", "Company", "Value")
      names(dt.for.second.year) = c("Project.Name", "Company", "Value")
      dt = rbind(dt.for.first.year, dt.for.second.year)
      
      
      Project = dt$Project.Name
      Company = dt$Company
      Year = character(length = nrow(dt))
      Month = rep.int(NA, nrow(dt))
      Value = dt$Value
      Unit.Def = rep.int("Barrels of Bitumen per day", nrow(dt))
      Product = rep.int("Bitumen", nrow(dt))
      Data.Source = rep.int(Data.Source.Name, nrow(dt))
      months.ended.type = rep.int(NA, nrow(dt))
      Time.Interval = rep.int(NA, nrow(dt))
      Data.Source.Year = rep.int(Data.Source.Year, nrow(dt))
      Data.Type = rep.int("Historical", nrow(dt))
      
      dt = data.frame(Project, Company, Year, Month, Value, Unit.Def, Product,
                              Data.Source, months.ended.type, Time.Interval, Data.Source.Year, Data.Type)
      
      
      ii = which(dt$Project == "Project")
      dt$Year[ii] = dt$Value[ii]
      i.three = which(grepl("Three Months Ended", dt$Project, ignore.case = TRUE))
      i.six = which(grepl("Six Months Ended", dt$Project, ignore.case = TRUE))
      i.nine = which(grepl("Nine Months Ended", dt$Project, ignore.case = TRUE))
      i.twelve = which(grepl("Twelve Months Ended", dt$Project, ignore.case = TRUE))
      
      dt$months.ended.type[i.three] = 3
      dt$months.ended.type[i.six] = 6
      dt$months.ended.type[i.nine] = 9
      dt$months.ended.type[i.twelve] = 12
      dt$months.ended.type = fill.from.below(dt$months.ended.type, initial = "0")
      dt$Time.Interval = dt$months.ended.type
      
      indices = list(i.three, i.six, i.nine, i.twelve)
      subtractor = c(1, 2, 4, 5)
      for (i.indices in 1:length(indices)) {
        for (i.months in 1:length(month.name)) {                   #month.name is a built-in constant
          rows.matching.month = grep(month.name[i.months], dt$Project , ignore.case = TRUE)
          rows.matching.month = intersect(rows.matching.month, indices[[i.indices]])
          dt$Month[rows.matching.month] = (i.months + 12 - subtractor[i.indices]) %% 12
        }
      }
      dt$Year[dt$Year == ""] = NA
      dt$Year = fill.from.below(dt$Year)
      dt$Month[dt$Month == ""] = NA
      dt$Month = fill.from.below(dt$Month)
      
      ij = which(!(dt$Value == "" | dt$Value == "---" | dt$Project == "Project" | 
        dt$Project == "Total In Situ" | dt$Project == "Total Mining" | dt$Project == "Grand Total"))
      
      dt = dt[ij,]
      Dates = standardize.date.list(cbind(dt$Year, dt$Month))
      dt = cbind(dt, Dates)
    }
    dt.new = dt[production.data.names]
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
  }
  
  if (source.type=="project_plan_2002_s_tdsecurities") {
    
    Data.Source.Name = "http://www.imperialinvestmentrealty.com/TD%20AlbertaOilSands.pdf"
    
    dt.names = c("Plan.Id","Project","Company.ID","Company.Name","Combined.Name",
                         "Attributed.Project","Stage","Costs","Status","AEUB.Status","Startup",
                         "Production","CumProduction","Upgrader","Description")
    dt = read.table(file.name, sep= ";", dec =".", as.is = TRUE, header = TRUE, fill = TRUE, col.names = dt.names)
    
    if (nrow(dt) > 0) {
      
      Project = dt$Project
      Project[Project == "Syncrude"] = "Mildred Lake/Aurora" # replacing misnomers 
      Project[Project == "Suncor"] = "Base Operations"       # replacing misnomers
      Company = dt$Company.Name
      Stage = dt$Stage
      Stage.Nr = rep.int("", nrow(dt))
      Type = character(length = nrow(dt))
      Costs = dt$Costs
      Operating.Status = dt$Status
      AEUB.Status = dt$AEUB.Status
      Startup = standardize.date.list(dt$Startup)
      Value = dt$Production
      Unit.Def = rep.int("Barrels of bitumen per day", nrow(dt))
      Description = dt$Description
      Descr.Updated = rep.int(gsub("[[:alpha:]_]","", file.name), nrow(dt))
      Descr.Updated = gsub(".", "", Descr.Updated, fixed = TRUE)
      Data.Source.Year = rep.int(gsub("[[:alpha:]_]","", file.name), nrow(dt))
      Data.Source.Year = gsub(".", "", Data.Source.Year, fixed = TRUE)
      Data.Source = rep.int(Data.Source.Name, nrow(dt))
      
      dt = data.frame(Project, Company, Stage, Stage.Nr, Type, Costs, Operating.Status, AEUB.Status, Startup,
                              Value, Unit.Def, Description, Descr.Updated, Data.Source.Year, Data.Source, 
                              stringsAsFactors = FALSE)
      
    }
    dt.new = dt
    dt.old = project.plan
    dt.new = rbind(dt.old, dt.new)
    
    project.plan = unique(dt.new)
    setwd("..")
    setwd("data")
    save(project.plan, file = "project.plan.rda")
    setwd("..")
    
  }
  
  if (source.type=="project_plan_s_oilsandsreview_updateable") {
    
    Data.Source.Name = "http://www.oilsandsreview.com"
    
    dt = read.table(file.name, sep= "\n", blank.lines.skip = FALSE)
    Year.row = grep("Year of last change", dt[,1])
    Data.Source.Year = gsub("Year of last change: ", "", dt[Year.row,1])
    
    # erase all unneeded quotation marks first
    write.table(gsub("[\'\"]", "", readLines(file(description = file.name))), file = file.name, 
                quote = FALSE, row.names = FALSE, col.names = FALSE)
    # now read the file without any problems
    dts.names = c("Company","Project","Phase","Updated","Description","Type","Startup",
                          "Value", "Costs","AEUB.Status")
    dt = read.table(file.name, sep= "\t", dec =".", as.is = TRUE, header = FALSE, fill = TRUE, 
                    col.names = dts.names, skip = Year.row)
    
    if (nrow(dt) > 0) {
      
      Project = dt$Project
      Company = dt$Company
      Stage = dt$Phase
      Stage.Nr = rep.int("", nrow(dt))
      Type = dt$Type
      Costs = dt$Costs
      Operating.Status = character(length = nrow(dt))
      AEUB.Status = dt$AEUB.Status
      Startup = dt$Startup
      Value = dt$Value
      Unit.Def = rep.int("Barrels of bitumen per day", nrow(dt))
      Description = dt$Description
      Descr.Updated = dt$Updated
      Data.Source.Year = rep.int(Data.Source.Year, nrow(dt))  
      Data.Source = rep.int(Data.Source.Name, nrow(dt))
      
      dt = data.frame(Project, Company, Stage, Stage.Nr, Type, Costs, Operating.Status, AEUB.Status, Startup,
                              Value, Unit.Def, Description, Descr.Updated, Data.Source.Year, Data.Source, 
                              stringsAsFactors = FALSE)
      
      
      dt$Company = fill.from.below(dt$Company, replace = "")
      dt$Project = fill.from.below(dt$Project, replace = "")
      dt$Descr.Updated = fill.from.below(dt$Descr.Updated, replace = "")
      dt$Description = fill.from.above(dt$Description, replace = "")

      dt = dt[which(dt$AEUB.Status != ""),]
      dt$Descr.Updated = gsub("Description:","", dt$Descr.Updated)
      dt$Descr.Updated = gsub(":","", dt$Descr.Updated)
      dt$Type = gsub("Type: ","", dt$Type)
      dt$Startup = gsub("Startup: ","", dt$Startup)
      dt$Startup = standardize.date.list(dt$Startup)
      dt$Value = gsub("bpd","", dt$Value)
      dt$Value = gsub(",","", dt$Value)
      
    }
    
    dt.old = project.plan
    dt.new = rbind(dt.old, dt)
    
    project.plan = unique(dt.new)
    setwd("..")
    setwd("data")
    save(project.plan, file = "project.plan.rda")
    setwd("..")
    
    
    return("Files converted. Find them in the subfolder /data.")
  }
  
  if (source.type=="capp_forecast_2006") {

    Data.Source = "CAPP Canadian Crude Oil Production and Supply Forecast 2006-2020"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    Year = dt[max(which(grepl("Year", dt[,1]))),]
    Year = strsplit(Year, " ")[[1]]
    Year = Year[Year != "Year"]
    Dates = standardize.date.list(Year)
    
    Value = dt[max(which(grepl("Oil Sands Mining", dt[,1]))),]
    Value = strsplit(Value, " ")[[1]]
    Value = Value[!is.na(as.numeric(Value))]
    Unit.Def = rep.int("Thousands of barrels bitumen per day", length(Value))
    Project = rep.int("Total Mining", length(Value))
    Company = rep.int("", length(Value))
    Dates = Dates
    Product = rep.int("Bitumen", length(Value))
    Data.Source = rep.int(Data.Source, length(Value))
    Time.Interval = rep.int("12", length(Value))
    Data.Source.Year = rep.int("2006", length(Value))
    Data.Type = rep.int("Prognosis", length(Value))
    
    dt.mining = data.frame(Project, Company, Dates, Value, 
                                   Unit.Def, Product, Data.Source, Time.Interval, Data.Source.Year, Data.Type)
    
    Value = dt[max(which(grepl("Oil Sands In-Situ", dt[,1]))),]
    Value = strsplit(Value, " ")[[1]]
    Value = Value[!is.na(as.numeric(Value))]
    Project = rep.int("Total In Situ", length(Value))
    Company = rep.int("", length(Value))
    Dates = Dates
    Unit.Def = rep.int("Thousands of barrels bitumen per day", length(Value))
    Product = rep.int("Bitumen", length(Value))
    Data.Source = rep.int(Data.Source, length(Value))
    Time.Interval = rep.int("12", length(Value))
    Data.Source.Year = rep.int("2006", length(Value))
    Data.Type = rep.int("Prognosis", length(Value))
    
    dt.in.situ = data.frame(Project, Company, Dates, Value, 
                                    Unit.Def, Product, Data.Source, Time.Interval, Data.Source.Year, Data.Type)
    
    dt.new = rbind(dt.mining, dt.in.situ)
    dt.new = dt.new[production.data.names]
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
    
  }
  
  if (source.type == "capp_forecast_2012") {
    
    Data.Source = "CAPP CANADIAN CRUDE OIL PRODUCTION FORECAST 2012-2030"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    Year = dt[max(which(grepl("Year", dt[,1]))),]
    Year = strsplit(Year, " ")[[1]]
    Year = Year[Year != "Year"]
    Dates = standardize.date.list(Year)
    
    Value = dt[max(which(grepl("Oil Sands Mining", dt[,1]))),]
    Value = strsplit(Value, " ")[[1]]
    Value = Value[!is.na(as.numeric(Value))]
    Project = rep.int("Total Mining", length(Value))
    Company = rep.int("", length(Value))
    Dates = Dates
    Unit.Def = rep.int("Thousands of barrels bitumen per day", length(Value))
    Product = rep.int("Bitumen", length(Value))
    Data.Source = rep.int(Data.Source, length(Value))
    Time.Interval = rep.int("12", length(Value))
    Data.Source.Year = rep.int("2012", length(Value))
    Data.Type = rep.int("Prognosis", length(Value))
    
    dt.mining = data.frame(Project, Company, Dates, Value, 
                                   Unit.Def, Product, Data.Source, Time.Interval, Data.Source.Year, Data.Type)
    
    Value = dt[max(which(grepl("Oil Sands In-Situ", dt[,1]))),]
    Value = strsplit(Value, " ")[[1]]
    Value = Value[!is.na(as.numeric(Value))]
    Project = rep.int("Total In Situ", length(Value))
    Company = rep.int("", length(Value))
    Dates = Dates
    Unit.Def = rep.int("Thousands of barrels bitumen per day", length(Value))
    Product = rep.int("Bitumen", length(Value))
    Data.Source = rep.int(Data.Source, length(Value))
    Time.Interval = rep.int("12", length(Value))
    Data.Source.Year = rep.int("2012", length(Value))
    Data.Type = rep.int("Prognosis", length(Value))
    
    dt.in.situ = data.frame(Project, Company, Dates, Value, 
                                    Unit.Def, Product, Data.Source, Time.Interval, Data.Source.Year, Data.Type)
    

    
    dt.new = rbind(dt.mining, dt.in.situ)
    dt.new = dt.new[production.data.names]
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
  }
  
  if (source.type == "capp_historical_2012") {
    Data.Source = "http://www.capp.ca/library/statistics/handbook/pages/statisticalTables.aspx?sectionNo=3"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    lines.to.skip = max(which(grepl("---", dt[,1])))
    dt = read.table(file.name, skip = lines.to.skip, header = TRUE, sep = ";")
    dt = dt[dt$Year != "Total",]
    
    # Same for all
    Dates = standardize.date.list(dt$Year)
    Company = ""
    Time.Interval = "12"
    Data.Source.Year = "2012"
    Data.Type = "Historical"
    Unit.Def = "Thousand Cubic Metres per year"
    
    # Mined.Synthetic.Crude
    Value = dt$Mined.Synthetic.Crude
    n = length(Value)
    Project = rep.int("Total Mining Synthetic Crude", n)
    Product = rep.int("Synthetic Crude", n)
    
    dt.Mined.Synthetic.Crude = data.frame(Dates, Value, Project, Product)
    
    # Mined.Bitumen
    Value = dt$Mined.Bitumen
    n = length(Value)
    Project = rep.int("Total Mining Bitumen", n)
    Product = rep.int("Bitumen", n)
    
    dt.Mined.Bitumen = data.frame(Dates,Value, Project, Product)
    
    # In.situ.Synthetic.Crude
    Value = dt$In.situ.Synthetic.Crude
    n = length(Value)
    Project = rep.int("Total In-situ Synthetic Crude", n)
    Product = rep.int("Synthetic Crude", n)
    
    dt.In.situ.Synthetic.Crude = data.frame(Dates, Value, Project, Product)
    
    # In.Situ.Experimental.and.Crude.Bitumen
    Value = dt$In.Situ.Experimental.and.Crude.Bitumen
    n = length(Value)
    Project = rep.int("Total In-Situ Bitumen", n)
    Product = rep.int("Bitumen", n)
    
    dt.In.Situ.Experimental.and.Crude.Bitumen = data.frame(Dates, Value, Project, Product)
    
    ### Collecting all the tables and adding constant values to every row
    dt.new = rbind(dt.Mined.Synthetic.Crude, dt.Mined.Bitumen, 
                           dt.In.situ.Synthetic.Crude, dt.In.Situ.Experimental.and.Crude.Bitumen)
    n = nrow(dt.new)
    Company= rep.int(Company, n)
    Unit.Def = rep.int(Unit.Def, n)
    Data.Source = rep.int(Data.Source, n)
    Time.Interval = rep.int(Time.Interval, n)
    Data.Source.Year = rep.int(Data.Source.Year, n)
    Data.Type = rep.int(Data.Type, n)
    
    dt.new = cbind(dt.new, Company, Unit.Def, Data.Source, Time.Interval, Data.Source.Year, Data.Type)
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
  } 
  
  if (source.type == "projection_aeub_2005") {
    
    Data.Source = "AEUB 2005"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    lines.to.skip = which(grepl("---", dt[,1]))
    
    Company = ""
    Unit.Def = "Thousand Cubic meters per day"
    Product = "Bitumen"
    Time.Interval = ""
    Data.Source.Year = "2005"
    Data.Type = "Prognosis"
                            
    # Mining.plus.In.Situ
    dt = read.table(file.name, header = TRUE, sep = ",", skip = lines.to.skip[1], 
                            nrows = (lines.to.skip[2] - lines.to.skip[1] - 2))
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Surface.Mining.plus.In.situ
    Project = rep.int("Total Mining plus In-Situ", length(Value))
    
    dt.Mining.plus.In.Situ = data.frame(Dates,Value,Project)
    
    # In.Situ
    dt = read.table(file.name, header = TRUE, sep = ",", skip = lines.to.skip[2])
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$In.Situ
    Project = rep.int("Total In-Situ", length(Value))
    
    dt.In.Situ = data.frame(Dates,Value,Project)
    
    dt.new = rbind(dt.Mining.plus.In.Situ, dt.In.Situ)
    
    n = nrow(dt.new)
    Company = rep.int(Company, n)
    Unit.Def = rep.int(Unit.Def, n)
    Product = rep.int(Product, n)
    Time.Interval = rep.int(Time.Interval, n)
    Data.Source.Year = rep.int(Data.Source.Year, n)
    Data.Type = rep.int(Data.Type, n)
    Data.Source = rep.int(Data.Source, n)
    
    dt.new = cbind(dt.new, Company, Unit.Def, Product, Time.Interval, 
                           Data.Source.Year, Data.Type, Data.Source)
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
    
                            
  } 
  
  if (source.type == "projection_neb_2003") {
    Data.Source = "Canada's Energy Future - scenarios for supply and demand to 2025"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    lines.to.skip = which(grepl("---", dt[,1]))
   
    Company = ""
    Unit.Def = "Thousand Cubic meters per day"
    Product = "Bitumen"
    Time.Interval = ""
    Data.Source.Year = "2003"
    Data.Type = "Prognosis"
    
    
    
    # In.Situ.Supply.Push
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[1], 
                            nrows = (lines.to.skip[2] - lines.to.skip[1] - 1))
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total In-Situ, Scenario Supply Push", length(Value))
    
    dt.In.Situ.Supply.Push = data.frame(Dates,Value,Project)
    
    # In.Situ.Techno.Vert
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[3], 
                            nrows = (lines.to.skip[4] - lines.to.skip[3] - 1))
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total In-Situ, Scenario Techno-Vert", length(Value))
    
    dt.In.Situ.Techno.Vert = data.frame(Dates,Value,Project)
    
    # Mining.Supply.Push
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[5], 
                            nrows = (lines.to.skip[6] - lines.to.skip[5] - 1))
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total Mining, Scenario Supply Push", length(Value))
    
    dt.Mining.Supply.Push = data.frame(Dates,Value,Project)
    
    # Mining.Techno.Vert
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[7])
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total Mining, Scenario Techno-Vert", length(Value))
    
    dt.Mining.Techno.Vert = data.frame(Dates,Value,Project)
    
    dt.new = rbind(dt.In.Situ.Supply.Push, dt.In.Situ.Techno.Vert,
                           dt.Mining.Supply.Push, dt.Mining.Techno.Vert)
    
    n = nrow(dt.new)
    Company = rep.int(Company, n)
    Unit.Def = rep.int(Unit.Def, n)
    Product = rep.int(Product, n)
    Time.Interval = rep.int(Time.Interval, n)
    Data.Source.Year = rep.int(Data.Source.Year, n)
    Data.Type = rep.int(Data.Type, n)
    Data.Source = rep.int(Data.Source, n)
    
    dt.new = cbind(dt.new, Company, Unit.Def, Product, Time.Interval, 
                           Data.Source.Year, Data.Type, Data.Source)
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
  } 
  
  if (source.type == "projection_neb_2007") {
    Data.Source = "Canada's Energy Future - scenarios for supply and demand to 2030"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    lines.to.skip = which(grepl("---", dt[,1]))
    
    Company = ""
    Unit.Def = "Thousand Cubic meters per day"
    Product = "Bitumen"
    Time.Interval = ""
    Data.Source.Year = "2007"
    Data.Type = "Prognosis"
    
    # Oil.Sands.Upgraded
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[1], 
                            nrows = (lines.to.skip[2] - lines.to.skip[1] - 1))
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total Oil Sands Upgraded", length(Value))
    
    dt.Oil.Sands.Upgraded = data.frame(Dates,Value,Project)
    
    # Oil.Sands.upgraded.PLUS.Non.Upgraded
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[3])
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total Oil Sands Upgraded Plus Non-Upgraded", length(Value))
    
    dt.Oil.Sands.upgraded.PLUS.Non.Upgraded = data.frame(Dates,Value,Project)
    
    
    dt.new = rbind(dt.Oil.Sands.Upgraded, dt.Oil.Sands.upgraded.PLUS.Non.Upgraded)
    
    n = nrow(dt.new)
    Company = rep.int(Company, n)
    Unit.Def = rep.int(Unit.Def, n)
    Product = rep.int(Product, n)
    Time.Interval = rep.int(Time.Interval, n)
    Data.Source.Year = rep.int(Data.Source.Year, n)
    Data.Type = rep.int(Data.Type, n)
    Data.Source = rep.int(Data.Source, n)
    
    dt.new = cbind(dt.new, Company, Unit.Def, Product, Time.Interval, 
                           Data.Source.Year, Data.Type, Data.Source)
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
    
    
  } 
  
  if (source.type == "projection_neb_2009") {
    Data.Source = "2009 Reference Case Scenario canadian energy demand and supply to 2020, p. 20"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    lines.to.skip = which(grepl("---", dt[,1]))
    
    Company = ""
    Unit.Def = "Thousand Cubic meters per day"
    Product = "Bitumen"
    Time.Interval = ""
    Data.Source.Year = "2009"
    Data.Type = "Prognosis"
    
    # Oil.Sands.Upgraded
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[1], 
                            nrows = (lines.to.skip[2] - lines.to.skip[1] - 1))
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total Oil Sands Upgraded", length(Value))
    
    dt.Oil.Sands.Upgraded = data.frame(Dates,Value,Project)
    
    # Oil.Sands.upgraded.PLUS.Non.Upgraded
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[3])
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total Oil Sands Upgraded Plus Non-Upgraded", length(Value))
    
    dt.Oil.Sands.upgraded.PLUS.Non.Upgraded = data.frame(Dates,Value,Project)
    
    
    dt.new = rbind(dt.Oil.Sands.Upgraded, dt.Oil.Sands.upgraded.PLUS.Non.Upgraded)
    
    n = nrow(dt.new)
    Company = rep.int(Company, n)
    Unit.Def = rep.int(Unit.Def, n)
    Product = rep.int(Product, n)
    Time.Interval = rep.int(Time.Interval, n)
    Data.Source.Year = rep.int(Data.Source.Year, n)
    Data.Type = rep.int(Data.Type, n)
    Data.Source = rep.int(Data.Source, n)
    
    dt.new = cbind(dt.new, Company, Unit.Def, Product, Time.Interval, 
                           Data.Source.Year, Data.Type, Data.Source)
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
    
    
  }
  
  if (source.type == "projection_ostr_2004") { 
    
    Data.Source = "Oil sands technology roadmap - unlocking the potential"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    lines.to.skip = which(grepl("---", dt[,1]))
    
    Company = ""
    Unit.Def = "Thousand barrels per day"
    Product = "Bitumen"
    Time.Interval = ""
    Data.Source.Year = "2004"
    Data.Type = "Prognosis"
    Project = "Total Oil Sands" 
    
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[1])
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    
    
    dt.new = data.frame(Dates,Value)
    
    n = nrow(dt.new)
    Project = rep.int(Project, n)
    Company = rep.int(Company, n)
    Unit.Def = rep.int(Unit.Def, n)
    Product = rep.int(Product, n)
    Time.Interval = rep.int(Time.Interval, n)
    Data.Source.Year = rep.int(Data.Source.Year, n)
    Data.Type = rep.int(Data.Type, n)
    Data.Source = rep.int(Data.Source, n)
    
    dt.new = cbind(dt.new, Project, Company, Unit.Def, Product, Time.Interval, 
                           Data.Source.Year, Data.Type, Data.Source)
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
    
  }
  
  if (source.type == "citibank_forecast_2012") {
 
    Data.Source = "Citibank - Energy 2020 - North America, the New Middle East?, page 26"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    lines.to.skip = which(grepl("---", dt[,1]))
    
    # First part: Total Light + Heavy + Atlantic + Oil Sands
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[1],
                            nrows = (lines.to.skip[2] - lines.to.skip[1] - 2), header = TRUE)
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total Light + Heavy + Atlantic + Oil Sands", length(Dates))
    
    dt.light.heavy.atlantic.oil.sands = data.frame(Dates, Value, Project)
    
    
    # Second part
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[2], header = TRUE)
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total Light + Heavy + Atlantic", length(Dates))
    
    dt.light.heavy.atlantic = data.frame(Dates, Value, Project)
    
    dt.new = rbind(dt.light.heavy.atlantic.oil.sands,
                           dt.light.heavy.atlantic)
    
    # add common columns
    n = nrow(dt.new)
    Company = rep.int("", n)
    Unit.Def = rep.int("Million barrels per day", n)
    Product = rep.int("Bitumen", n)
    Time.Interval = rep.int("", n)
    Data.Source.Year = rep.int("2012", n)
    Data.Type = rep.int("Prognosis", n)
    Data.Source = rep.int(Data.Source, n)
    
    
    dt.new = cbind(dt.new, Company, Unit.Def, Product, Time.Interval, 
                           Data.Source.Year, Data.Type, Data.Source)
    dt.new = dt.new[production.data.names]
  
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
    
  }
  
  if (source.type=="canada_stat_hist_2012") {
    
    Data.Source = "Energy Statistics Handbook - First quarter 2012"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    lines.to.skip = which(grepl("---", dt[,1]))
    
    dt = read.table(file.name, sep = " ", skip = lines.to.skip[2],
                            nrows = (lines.to.skip[3] - lines.to.skip[2] - 1))
    names(dt) = read.table(file.name, sep = ",", skip = lines.to.skip[1], nrows = 1)
    
    
    dt.new = data.frame(row.names = c("Dates", "Value", "Project", "Product"))
    for (column in names(dt)[2:length(names(dt))]) {
      Dates = standardize.date.list(dt$Year)
      Value = dt[,column]
      Project = rep.int(column, length(Dates))
      Product = rep.int(column, length(Dates))
      dt.new = rbind(dt.new, data.frame(Dates, Value, Project, Product))
    }
    Time.Interval = rep.int("12", nrow(dt.new))
    Unit.Def = rep.int("Thousands of cubic metres per year", nrow(dt.new))
    dt.all = cbind(dt.new, Time.Interval, Unit.Def)
    
    dt = read.table(file.name, sep = " ", skip = lines.to.skip[4],
                            nrows = (lines.to.skip[5] - lines.to.skip[4] - 1))
    names(dt) = read.table(file.name, sep = ",", skip = lines.to.skip[1], nrows = 1)
    Dates.matrix = matrix(c(rep.int(2011, 12), 1:12), ncol = 2)
    dt$Year = standardize.date.list(Dates.matrix)
    
    dt.new = data.frame(row.names = c("Dates", "Value", "Project", "Product"))
    for (column in names(dt)[2:length(names(dt))]) {
      Dates = dt$Year
      Value = dt[,column]
      Project = rep.int(column, length(Dates))
      Product = rep.int(column, length(Dates))
      dt.new = rbind(dt.new, data.frame(Dates, Value, Project, Product))
    }
    Time.Interval = rep.int("1", nrow(dt.new))
    Unit.Def = rep.int("Thousands of cubic metres per month", nrow(dt.new))
    dt.new = cbind(dt.new, Time.Interval, Unit.Def)
    
    dt.all = rbind(dt.all, dt.new)
    
    n = nrow(dt.all)
    Company = rep.int("", n)
    Data.Source.Year = rep.int("2012", n)
    Data.Type = rep.int("Historical", n)
    Data.Source = rep.int(Data.Source, n)
    
    dt.all = cbind(dt.all, Company, Data.Source.Year, Data.Type, Data.Source)
    
    dt.new = dt.all[production.data.names]

    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)

    save(production.data, file = "../data/production.data.rda")
    
  }
  
  if (source.type == "projection_neb_2000") {
    
    Data.Source = "Canada's Oil Sands: A Supply and Market Outlook to 2015"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE, quote ="#")
    
    lines.to.skip = which(grepl("---", dt[,1]))
    
    Company = ""
    Unit.Def = "Thousand Cubic meters per day"
    Product = "Bitumen"
    Time.Interval = ""
    Data.Source.Year = "2000"
    Data.Type = "Prognosis"
    
    
    
    # In.Situ $22 Sensitivity
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[2], 
                            nrows = (lines.to.skip[3] - lines.to.skip[2] - 2), header = TRUE)
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total In-Situ, Scenario $22 Sensitivity", length(Value))
    
    dt.In.Situ.22.dollar = data.frame(Dates,Value,Project)
    
    # In.Situ $14 Sensitivity
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[4], 
                            nrows = (lines.to.skip[5] - lines.to.skip[4] - 1))
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total In-Situ, Scenario $14 Sensitivity", length(Value))
    
    dt.In.Situ.14.dollar = data.frame(Dates,Value,Project)
    
    # In.Situ. $18 Base Case
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[6], 
                            nrows = (lines.to.skip[7] - lines.to.skip[6] - 1))
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total In-Situ, Scenario $18 Base Case", length(Value))
    
    dt.In.Situ.18.dollar = data.frame(Dates,Value,Project)

    # Mining. $22 Sensitivity
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[8], 
                            nrows = (lines.to.skip[9] - lines.to.skip[8] - 1))
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total Mining, Scenario $22 Sensitivity", length(Value))
    
    dt.Mining.22.dollar = data.frame(Dates,Value,Project)
    
    # Mining. $14 Sensitivity
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[10], 
                            nrows = (lines.to.skip[11] - lines.to.skip[10] - 1))
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total Mining, Scenario $14 Sensitivity", length(Value))
    
    dt.Mining.14.dollar = data.frame(Dates,Value,Project)
    
    # Mining Scenario $18 Base Case
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[12])
    names(dt) = c("Year", "Value")
    Dates = standardize.date.list(dt$Year, format = "decimal")
    Value = dt$Value
    Project = rep.int("Total Mining, Scenario $18 Base Case", length(Value))
    
    dt.Mining.18.dollar = data.frame(Dates,Value,Project)
    
    dt.new = rbind(dt.In.Situ.22.dollar, dt.In.Situ.14.dollar, dt.In.Situ.18.dollar,
                           dt.Mining.22.dollar, dt.Mining.14.dollar, dt.Mining.18.dollar)
    
    n = nrow(dt.new)
    Company = rep.int(Company, n)
    Unit.Def = rep.int(Unit.Def, n)
    Product = rep.int(Product, n)
    Time.Interval = rep.int(Time.Interval, n)
    Data.Source.Year = rep.int(Data.Source.Year, n)
    Data.Type = rep.int(Data.Type, n)
    Data.Source = rep.int(Data.Source, n)
    
    dt.new = cbind(dt.new, Company, Unit.Def, Product, Time.Interval, 
                           Data.Source.Year, Data.Type, Data.Source)
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
  }
  
  if (source.type == "CERI_2003") {
    Data.Source = "CERI - Oil Sands Supply Outlook 2003-2017"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE, quote ="#")
        
    
    lines.to.skip = which(grepl("---", dt[,1]))
    
    dt = read.table(file.name, sep = " ", skip = lines.to.skip[3], 
                            nrows = (lines.to.skip[4] - lines.to.skip[3] - 1), header = FALSE)
    Project = rep.int("Unconstrained Supply", nrow(dt))
    dt.unconstrained = cbind(dt, Project)
    
    dt = read.table(file.name, sep = " ", skip = lines.to.skip[5], 
                            nrows = (lines.to.skip[6] - lines.to.skip[5] - 1), header = FALSE)
    Project = rep.int("Adjusted Unconstrained Supply", nrow(dt))
    dt.adj.unconstrained = cbind(dt, Project)
    
    dt = read.table(file.name, sep = " ", skip = lines.to.skip[7], 
                    nrows = (lines.to.skip[8] - lines.to.skip[7] - 1), header = FALSE)
    Project = rep.int("High Supply Outlook", nrow(dt))
    dt.high.supply = cbind(dt, Project)
    
    dt = read.table(file.name, sep = " ", skip = lines.to.skip[9], 
                    nrows = (lines.to.skip[10] - lines.to.skip[9] - 1), header = FALSE)
    Project = rep.int("Reference Supply Outlook", nrow(dt))
    dt.reference = cbind(dt, Project)
    
    dt = read.table(file.name, sep = " ", skip = lines.to.skip[11], header = FALSE)
    Project = rep.int("Low Supply Outlook", nrow(dt))
    dt.low.supply = cbind(dt, Project)
    
    dt = rbind(dt.unconstrained, dt.adj.unconstrained, dt.high.supply, dt.reference, dt.low.supply)
    names(dt) = c(read.table(file.name, sep = ";", skip = lines.to.skip[1], nrows = 1), "Project")
    
    col.indices = intersect(grep("Project", names(dt), invert = TRUE), grep("Year", names(dt), invert = TRUE))
    dt.new = data.frame(Year = c(), Value = c(), Project = c(), Product = c())
    for (col.name in names(dt)[col.indices]) {
      Year = dt$Year
      Value = as.vector(dt[col.name])
      names(Value) = "Value"
      for (ii in 1:nrow(Value)) {
        Project[ii] = paste(dt$Project[ii], col.name, collapse = " - ")
      }
      Product = rep.int(col.name, nrow(Value))
      dt.new = rbind(dt.new, data.frame(Year, Value = Value, Project, Product))
    }
    n = nrow(dt.new)
    Unit.Def = rep.int("Thousand barrels per day", n)
    Company = rep.int("", n)
    Dates = standardize.date.list(dt.new["Year"])
    Data.Source = rep.int(Data.Source, n)
    Time.Interval = rep.int("12", n)
    Data.Source.Year = rep.int("2004", n)
    Data.Type = rep.int("Prognosis", n)
    
    dt.new = cbind(dt.new, Unit.Def, Company, Dates, Data.Source, Time.Interval, Data.Source.Year, Data.Type)
    
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
  }
  
  if (source.type == "CERI_2011") {
    Data.Source = "CERIs Oil Sands Production Forecast (Study 122) "
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    lines.to.skip = which(grepl("---", dt[,1]))
    
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[1],
                    nrows = (lines.to.skip[2] - lines.to.skip[1] - 1), header = FALSE)
    Project = rep.int("Protracted", nrow(dt))
    dt.Protracted = cbind(dt, Project)
    
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[3],
                    nrows = (lines.to.skip[4] - lines.to.skip[3] - 1), header = FALSE)
    Project = rep.int("Security", nrow(dt))
    dt.Security = cbind(dt, Project)
    
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[5],
                    nrows = (lines.to.skip[6] - lines.to.skip[5] - 1), header = FALSE)
    Project = rep.int("Unconstrained", nrow(dt))
    dt.Unconstrained = cbind(dt, Project)
    
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[7])
    Project = rep.int("Realistic", nrow(dt))
    dt.Realistic = cbind(dt, Project)
    
    dt.new = rbind(dt.Protracted, dt.Security, dt.Unconstrained,dt.Realistic)
    
    n = nrow(dt.new)
    Project = dt.new["Project"]
    Company = rep.int("", n)
    Dates = standardize.date.list(dt.new[,1], format = "decimal")           
    Value = dt.new[,2]   
    Unit.Def = rep.int("Thousand barrels per day", n)        
    Product = rep.int("Bitumen", n)          
    Data.Source = rep.int(Data.Source, n)      
    Time.Interval = rep.int("", n)    
    Data.Source.Year = rep.int("2011", n) 
    Data.Type = rep.int("Prognosis", n)
    
    dt.new = data.frame(Project, Company, Dates, Value, Unit.Def, Product, Data.Source, Time.Interval, 
                        Data.Source.Year, Data.Type)
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
  }
  
  if (source.type == "CERI_2011_status") {
    Data.Source = "Canadian Oil Sands Supply Costs and Development Projects (2010-2044), p 40"
    dt = read.table(file.name, sep = "\n", blank.lines.skip = FALSE)
    
    lines.to.skip = which(grepl("---", dt[,1]))
    
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[1],
                    nrows = (lines.to.skip[2] - lines.to.skip[1] - 1), header = FALSE)
    Project = rep.int("Announced+Awaiting Approval+Suspended+Approved+Construction+Onstream", nrow(dt))
    dt.1 = cbind(dt, Project)
    
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[3],
                    nrows = (lines.to.skip[4] - lines.to.skip[3] - 1), header = FALSE)
    Project = rep.int("Awaiting Approval+Suspended+Approved+Construction+Onstream", nrow(dt))
    dt.2 = cbind(dt, Project)
    
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[5],
                    nrows = (lines.to.skip[6] - lines.to.skip[5] - 1), header = FALSE)
    Project = rep.int("Suspended+Approved+Construction+Onstream", nrow(dt))
    dt.3 = cbind(dt, Project)
    
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[7],
                    nrows = (lines.to.skip[8] - lines.to.skip[7] - 1), header = FALSE)
    Project = rep.int("Approved+Construction+Onstream", nrow(dt))
    dt.4 = cbind(dt, Project)
    
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[9],
                    nrows = (lines.to.skip[10] - lines.to.skip[9] - 1), header = FALSE)
    Project = rep.int("Construction+Onstream", nrow(dt))
    dt.5 = cbind(dt, Project)
    
    dt = read.table(file.name, sep = ",", skip = lines.to.skip[11])
    Project = rep.int("Onstream", nrow(dt))
    dt.6 = cbind(dt, Project)
    
    dt.new = rbind(dt.1, dt.2, dt.3, dt.4, dt.5, dt.6)
    
    n = nrow(dt.new)
    Project = dt.new["Project"]
    Company = rep.int("", n)
    Dates = standardize.date.list(dt.new[,1], format = "decimal")           
    Value = dt.new[,2]   
    Unit.Def = rep.int("Thousand barrels per day", n)        
    Product = rep.int("Bitumen", n)          
    Data.Source = rep.int(Data.Source, n)      
    Time.Interval = rep.int("", n)    
    Data.Source.Year = rep.int("2011", n) 
    Data.Type = rep.int("Prognosis", n)
    
    dt.new = data.frame(Project, Company, Dates, Value, Unit.Def, Product, Data.Source, Time.Interval, 
                        Data.Source.Year, Data.Type)
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
  }
  
  if (source.type == "projection_neb_2011") {
    Data.Source = "Canadas Energy Future - Energy Supply and Demand Projections to 2035, Table A3.31"
    
    dt = read.table(file.name, sep = "\t", blank.lines.skip = FALSE)
    
    Dates.row.max = 0
     for(ii in 1:nrow(dt)) {
       Dates.row.match = length(grep("20.", as.character(dt[ii,])))
       Mined.row.match = any(grepl("Mined", as.character(dt[ii,])))
       In.Situ.row.match = any(grepl("Situ", as.character(dt[ii,])))
       if (Mined.row.match) {Mined.row = ii}
       if (In.Situ.row.match) {In.Situ.row = ii}
       if (Dates.row.match > Dates.row.max) {
         Dates.row = ii
         Dates.row.max = Dates.row.match
       }
    }
    Dates = standardize.date.list(as.character(dt[Dates.row,]))
    Dates = Dates[!is.na(Dates)]
    
    #Mined Bitumen
    Value = dt[Mined.row,]
    Value = as.numeric(Value[!is.na(as.numeric(Value))])
    Project = rep.int("Total Mining", length(Value))
    
    dt.Mining = data.frame(Dates, Project, Value)
    
    #In Situ Bitumen
    Value = dt[In.Situ.row,]
    Value = as.numeric(Value[!is.na(as.numeric(Value))])
    Project = rep.int("Total In-Situ", length(Value))
    
    dt.In.Situ = data.frame(Dates, Project, Value)
    
    dt = rbind(dt.Mining, dt.In.Situ)
    
    n = nrow(dt)
    Company = rep.int("", n)
    Unit.Def = rep.int("thousand cubic metres per day", n)
    Product = rep.int("Bitumen", n)
    Data.Source = rep.int(Data.Source, n)
    Time.Interval = rep.int("12", n)
    Data.Source.Year = rep.int("2011", n)
    Data.Type = rep.int("Prognosis", n)
    
    dt.new = cbind(dt, Company, Unit.Def, Product, Data.Source, Time.Interval, Data.Source.Year, Data.Type)
    
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
    
  }
  
  if (source.type == "WEO_2012") {

    Data.Source = "World Energy Outlook 2012, page 107, Table 3.5"
    
    dt = read.table(file.name, sep= "\n", blank.lines.skip = FALSE)
    Date.row = grep("Year", dt[,1])
    Value.row = grep("Value", dt[,1])
    
    Dates = strsplit(dt[Date.row,1], " ")[[1]]
    Dates = standardize.date.list(Dates[Dates != "Year"])
    
    Value = strsplit(dt[Value.row,1], " ")[[1]]
    Value = Value[Value != "Value"]
    
    dt = data.frame(Dates, Value)
    
    n = nrow(dt)
    Project = rep.int("Total Bitumen", n)
    Company = rep.int("", n)
    Unit.Def = rep.int("Million barrels per day", n)
    Product = rep.int("Bitumen", n)
    Data.Source = rep.int(Data.Source, n)
    Time.Interval = rep.int("", n)
    Data.Source.Year = rep.int("2012", n)
    Data.Type = rep.int("Prognosis", n)
    
    dt.new = cbind(dt, Project, Company, Unit.Def, Product, Data.Source, Time.Interval, Data.Source.Year, Data.Type)
    
    dt.new = dt.new[production.data.names]
    
    dt.old = production.data[production.data.names]
    dt.new = rbind(dt.old, dt.new)
    
    production.data = unique(dt.new)
    setwd("..")
    setwd("data")
    save(production.data, file = "production.data.rda")
    setwd("..")
    
    
  }
  
  quick.load()
}