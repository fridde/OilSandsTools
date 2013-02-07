update.attribution = function(part = 1, new.projects.added = TRUE) {
  
  setwd(main.dir)
  
  if (part == 1 && new.projects.added) {
    new.attribution.table = attribution.table
    attribution.table.production = production.data[c("Project", "Company", "Product", "Data.Source", "Data.Source.Year", "Data.Type")] 
    Data.Table.Type = rep.int("production.data", nrow(attribution.table.production))
    attribution.table.production = cbind(attribution.table.production, Data.Table.Type)
    
    attribution.table.projects = project.plan[c("Project", "Company", "Data.Source", "Data.Source.Year")]
    names(attribution.table.projects) = c("Project", "Company", "Data.Source", "Data.Source.Year")
    n = nrow(attribution.table.projects)
    Product = rep.int("Bitumen", n) 
    Data.Table.Type = rep.int("project.plan", n)
    Data.Type = rep.int("Prognosis", n)
    attribution.table.projects = cbind(attribution.table.projects, Product, Data.Type, Data.Table.Type)
    
    new.attribution.table = rbind(attribution.table.production, attribution.table.projects[,names(attribution.table.production)])
    
    new.attribution.table = unique(new.attribution.table)
    
    Main.Compilation = rep.int("", nrow(new.attribution.table))
    columns.to.include = names(attribution.table)[names(attribution.table) != "Main.Compilation"]
    
    for (ii in 1:nrow(new.attribution.table)) {
      Main.Compilation[ii] = attribute.project(paste(new.attribution.table[ii,columns.to.include]), columns.to.include)
      if (is.na(Main.Compilation[ii])) {
        Main.Compilation[ii] = paste(new.attribution.table[ii, ], sep = "", collapse = " - ")
      }
    }
    attributions.raw.table = cbind(Main.Compilation, new.attribution.table)
    attributions.raw.table = unique(attributions.raw.table)
    
    write.table(attributions.raw.table, file = "sources/attributions_raw.txt", append = FALSE, quote = TRUE, sep = "\t",
                eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
  }
  
  if (part == 2) {
   
    if(new.projects.added) {
      attribution.table = read.table("sources/attributions_raw_reconciled.txt", 
                                     sep= "\t", as.is = TRUE, header = TRUE, quote = "\"")
      save(attribution.table, file = "data/attribution.table.rda")
    }
    
    quick.load()
    
    # update Main.Compilation in production.data
    columns.to.include = c("Project", "Company", "Product", "Data.Source", "Data.Source.Year", "Data.Type")
    
    if (is.null(production.data$Main.Compilation)) {
      Main.Compilation = rep.int("", nrow(production.data))
      production.data = cbind(Main.Compilation, production.data)
      save(production.data, file = "data/production.data.rda")
    }
    for (i.data.row in 1:nrow(production.data)) {
      production.data$Main.Compilation[i.data.row] = attribute.project(paste(production.data[i.data.row,columns.to.include]), columns.to.include) 
    }
    save(production.data, file = "data/production.data.rda")
    
    # update Main.Compilation in project.plan
    columns.to.include = c("Project", "Company", "Data.Source", "Data.Source.Year")
    
    if (is.null(project.plan$Main.Compilation)) {
      Main.Compilation = rep.int("", nrow(project.plan))
      project.plan = cbind(Main.Compilation, project.plan)
      save(project.plan, file = "data/project.plan.rda")
    }
    
    for (i.data.row in 1:nrow(project.plan)) {
      project.plan$Main.Compilation[i.data.row] = attribute.project(paste(project.plan[i.data.row,columns.to.include]) , columns.to.include)
    }
    save(project.plan, file = "data/project.plan.rda")
  }  
  quick.load()
}


