return.missing.Type <-
  function (Project.vector, Company.vector){
    # Project and Company should be given in standardized form
    #browser(text="return.missing.Type")
    Type.vector = c()
    project.plan.Project =  standardize.name(project.plan$Project, type = "Project")
    project.plan.Company = standardize.name(project.plan$Company, type = "Company")
    
    for (ii in 1:length(Project.vector)) {
      # do both project and company correspond? in this case draw the type from project.plan$type
      Type.value = project.plan$Type[intersect(which(project.plan.Project == Project.vector[ii]), which(project.plan.Company == Company.vector[ii]))]
      
      if (length(Type.value > 0)) {
        Type.vector = c(Type.vector, names(table(Type.value))[table(Type.value) == max(table(Type.value))][1])
      } 
      else {
        Type.vector = c(Type.vector,  "")
      }
    }
    
    return(Type.vector)
  }
