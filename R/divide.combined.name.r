divide.combined.name = function (Combined.Name.vector) {
  
  Project.vector = rep.int("", length(Combined.Name.vector))
  Company.vector = rep.int("", length(Combined.Name.vector))
  
  synonym.names.projects = synonym.names[synonym.names$Type == "Project" , ]
  synonym.names.companies = synonym.names[synonym.names$Type == "Company" , ]
  
  for (ii in 1:nrow(synonym.names.projects)) {
    i.project.found = grep(synonym.names.projects$Alias.Name[ii], Combined.Name.vector)
    Project.vector[i.project.found] = synonym.names.projects$Alias.Name[ii]
  }
  for (ii in 1:nrow(synonym.names.projects)) {
    i.company.found = grep(synonym.names.companies$Alias.Name[ii], Combined.Name.vector)
    Company.vector[i.company.found] = synonym.names.companies$Alias.Name[ii]
  }
  
  still.empty = union(which(Project.vector == ""), which(Company.vector == ""))     
  Combined.Names.to.seperate = Combined.Name.vector[still.empty]
  vector.to.fix = unique(grep(";", Combined.Names.to.seperate, invert = TRUE, value = TRUE))
  fixed.vector = c()
  
  # last resort: split manually by inserting ";" between Project and Company
  # 1. define where the split should happen
  while (length(vector.to.fix) > 0) {
    vector.to.fix = fix(vector.to.fix)
    fixed.vector = c(fixed.vector, grep(";", vector.to.fix, invert = FALSE, value = TRUE))
    vector.to.fix = grep(";", vector.to.fix, invert = TRUE, value = TRUE)
  }
  # 2. split all other elements in the same way
  for(i.fix in 1:length(fixed.vector)){
    Combined.Names.to.seperate[Combined.Names.to.seperate == gsub(";", "", fixed.vector[i.fix])]  = fixed.vector[i.fix]
  }       
  Combined.Name.vector[still.empty] = Combined.Names.to.seperate
  
  for (ii  in still.empty) {
    Project.vector[ii] = strsplit(Combined.Name.vector[ii], ";")[[1]][2]
    Company.vector[ii] = strsplit(Combined.Name.vector[ii], ";")[[1]][1]
  }
  # remove trailing or ending space
  Project.vector = gsub("^[ \t]+|[ \t]+$", "", Project.vector)
  Company.vector = gsub("^[ \t]+|[ \t]+$", "", Company.vector)
  
  value= list(Project.vector, Company.vector)
  return(value)
}