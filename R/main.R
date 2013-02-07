# main program
# source("R/main.r")
# main.dir = "C:/Users/Friedrich.Hehl/Dropbox/exjobbet/calculations/OilSandsTools/"
main = function () {
  rm(list = ls())
  main.dir = "C:/Users/Friedrich.Hehl/Dropbox/exjobbet/calculations/OilSandsTools/"
  #main.dir = "E:/Dropbox/exjobbet/calculations/OilSandsTools/"
  setwd(main.dir)
  source("R/quick.load.r")
  quick.load()
  #update.databases()
  #calculate.correlations()
  #dataset = compile.prod.proj()
  #save(dataset, file = "data/dataset.rda")
  
  predict.values()
  create.plot.collection()
 
  
#   dataset = compile.prod.proj(start.year = "1960", end.year = "1990")
#   #save(dataset, file = "data/dataset.rda")
#   for (scheme in schemes[5:length(schemes)]) {
#     filtered.dataset = filter.dataframe(dataset, scheme)
#     multi.line.plot(filtered.dataset, scheme)
#   }
#   
#   sample.frame = extract.sample.coefficients(interpolated.dataset)
#   hist(sample.frame$Quota.Plan.2002)
#   hist(sample.frame$Quota.Plan.2012)
#   attach(sample.frame)
#   plot(Date, Quota.Plan.2002)
#   lines(Date, Quota.Plan.2002)
#   write.table(sample.frame, file = "sample.frame.txt", sep = ";", na = "", row.names = FALSE)
#   
#   
#   
#   
#   all.date.values = extract.all.dates()
#   for (ii in 1:length(all.date.values)) {
#     cat(ii, " ", select.projects(all.date.values[ii], 2002), "---")
#   }
#   
#   filename1 = "production_data_s_environment.txt"
#   filename2 = "production_data_s_oilsandsreview.txt"
#   filename3 = "project_plan_2002_s_tdsecurities.txt"
#   filename4 = "project_plan_s_oilsandsreview.txt"
#   conv.file1 = "project_plan.txt"
#   conv.file2 = "production_data.txt"
#   list.of.project.files.with.names = list(conv.file1) 
#   list.of.production.files.with.names = list(conv.file2)
#   synonym.names.first.part(list.of.project.files.with.names, list.of.production.files.with.names)
#   SQLify(filename1)
#   SQLify(filename2)
#   SQLify(filename3)
#   SQLify(filename4)
#   
#   
#   
#   source("prepare_all.R")
#   source("comparison.R")
#   original.dir = "C:/Users/Friedrich.Hehl/Google Drive/GlobalEnergySystems/from_refine"
#   copy.dir = "C:/Users/Friedrich.Hehl/Dropbox/exjobbet/calculations/from_refine"
#   prepared.data = prepare.all(original.dir, copy.dir)
#   names(prepared.data) = c("Projects.Data", "list.of.tables", "list.plans", "list.years")
#   attach(prepared.data, warn.conflicts = FALSE)
#   attach(list.of.tables, warn.conflicts = FALSE)
#   all.dates = extract.documented.dates(Production_by_Project, list.of.tables, list.plans)
#   collection = time.collector(all.dates, start.time = "2000-01-01", end.time = "2050-01-01")
#   
#   
#   
#   ### works until here! date: 120902
#   
#   # test run, works! (seems so, check numbers!)
#   collection = prod.collector(Projects.Data, collection, Production_by_Project, check.factor ="Type",
#                               check.factor.attribute = "." , Projects_Directory)
#   names(collection) =c("all.dates", "production")
#   collection = plan.collector(prepared.data, list.plans, list.of.tables, collection, check.factor ="Stage",
#                               check.factor.attribute = ".")
#   
#   
#   #collection = subset(collection, (collection$production != "NA") && (collection$Plan_Year_Project_Plan_2004 != "NA"))
#   collection$all.dates = as.POSIXlt(collection$all.dates, origin = "1970-01-01 CET")
#   plot(splines(collection$all.dates, collection$Plan_Year_Project_Plan_2004), type ="l", col = "steelblue", ylim = c(0,7000000))
#   lines(splines(collection$all.dates, collection$production), col="red")
#   lines(splines(collection$all.dates, collection$Plan_Year_Project_Plan_aug2012), col= " red2")
#   
#   #xlim=c(min(collection$all.dates), max(collection$all.dates))
#   result = c()
#   for (ii in 1:length(b)) {
#     result = c(result, convert.to.barrel.per.day(b[ii], a[ii]))
#   }
}