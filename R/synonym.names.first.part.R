synonym.names.first.part <- function() {
  # ----------------------------------------
  # Prepares for matchching every project- and company name with their corresponding official name----------------------
  # Has to be complemented with a manual matching using GoogleRefine
  # ----------------------------------------    
  
  
  # creates a table with synonym names of both Projects and Companies
  
  
  all.Project.names = c(production.data$Project, project.plan$Project)
  all.Company.names = c(production.data$Company, project.plan$Company)
  
  new.Project.names = all.Project.names[which(is.na(standardize.name(all.Project.names, type = "Project")))]
  new.Company.names = all.Company.names[which(is.na(standardize.name(all.Company.names, type = "Company")))]
  
  if (length(c(new.Project.names, new.Company.names)) > 0) {
    
    old.Project.names = synonym.names$Alias.Name[synonym.names$Type == "Project"]
    old.Company.names = synonym.names$Alias.Name[synonym.names$Type == "Company"]
   
    Project.names = sort(unique(c(old.Project.names, new.Project.names)))
    Company.names = sort(unique(c(old.Company.names, new.Company.names)))
    # to equalize lengths
    if (length(Project.names) > length(Company.names)) {
      Company.names = c(Company.names, rep.int("", length(Project.names) - length(Company.names)) ) 
    }
    if (length(Project.names) < length(Company.names)) {
      Project.names = c(Project.names, rep.int("", length(Company.names) - length(Project.names)) )
    }
    
    Project.Type = rep.int("Project", length(Project.names)) 
    Company.Type = rep.int("Company",length(Company.names))
    
    Main.Project.Name = standardize.name(Project.names, type = "Project")
    Main.Project.Name[which(is.na(Main.Project.Name))] = Project.names[which(is.na(Main.Project.Name))]
    Main.Company.Name = standardize.name(Company.names, type = "Company")
    Main.Company.Name[which(is.na(Main.Company.Name))] = Company.names[which(is.na(Main.Company.Name))]
    
    synonym.table = data.frame(Main.Project.Name, Synonym.Project.Name = Project.names,
                               Project.Type, Main.Company.Name, Synonym.Company.Name = Company.names, 
                               Company.Type)
    
    setwd("sources")
    write.table(synonym.table, file = "synonym_names_raw.txt", append = FALSE, quote = TRUE, sep = ";",
                eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
    setwd("..")
  }
}
