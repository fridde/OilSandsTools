update.databases = function(with.manual.input = FALSE) {
  
  
  # load all funtions and datasets
  rm(list = ls())
  main.dir = "C:/Users/Friedrich.Hehl/Dropbox/exjobbet/calculations/OilSandsTools/"
  setwd(main.dir)
  source("R/quick.load.r")
  quick.load()
  
  production.data = production.data[is.na(production.data$Dates),]  # dummy index to empty the table
  save(production.data, file = "data/production.data.rda")
  project.plan = project.plan[is.na(project.plan$Main.Compilation),] # dummy index to empty the table
  save(project.plan, file = "data/project.plan.rda")

  # enters the values of the text-files in the folder "sources" into production.data and project.plan
  # Requires text files to be manually created by copy-and-paste described in the manual
  if(exists("with.manual.input") && with.manual.input) {
    files.to.update = list.files(path= "sources/", pattern = "updateable\\.txt")
    manual.input(files.to.update)
  }
  
  files.to.update = list.files(path= "sources/", pattern = ".txt")
  for(ii in 1:length(files.to.update)){
    file.name = paste(files.to.update[ii])
    SQLify(file.name)
  }
  
  
  # -----------------------------------------------------------------------------------------------------
  # create synonym-tables. do only if new Companies or detailed project names are introduced 
#   synonym.names.first.part()
#   output.text = c("Now please follow the steps given in the chapter *Reconciling the synonym table* in the Data COllection Manual")
#   cat(output.text)
#   is.updated = FALSE
#   while (!is.updated) {
#     is.updated = grepl("yes", varEntryDialog(vars = "Write Yes", title = "Did you reconcile and export the synonym_names_raw_reconciled.txt file?")[[1]], 
#                        ignore.case = TRUE)
#   }
#   synonym.names.second.part()
  
  # -----------------------------------------------------------------------------------------------------
  # create attribution tables. This is mandatory
  update.attribution(part = 1)
  #is.updated = FALSE
  #while (!is.updated) {
  #  is.updated = grepl("yes", varEntryDialog(vars = "Write Yes", title = "Did you reconcile and export the attributions_raw_reconciled.txt file?")[[1]], 
  #                     ignore.case = TRUE)
  #}
  update.attribution(part = 2)
  #--------------------------------------------------------------
  
  # updates project stages. 
  update.project.stage(part = 1)
  # Now open temp/stage_numbers_raw.txt with Excel and edit the column "Stage.Nr" so that all cells are filled.
  # In most cases this will just be integers that increase with +1 for every cell, but sometimes human guesses are needed
  # Save the file after editing
  update.project.stage(part = 2)  
  # -----------------------------------------------------------------------------------------------------
  
  quick.load()
  # update all datasets and vectors
  all.the.dates = extract.all.dates()
  save(all.the.dates, file = "data/all.the.dates.rda")
  
}