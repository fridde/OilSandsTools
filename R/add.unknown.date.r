add.unknown.date = function(delay.value = 3) {
  
  project.plan = project.plan[order(project.plan$Project, project.plan$Company, project.plan$Startup),]
  
  for (ii in 1:nrow(project.plan)) {
    if(!(project.plan$has.Startup.date[ii])) {
   
      if (project.plan$Stage.Nr[ii] == "1") {
        new.date = as.POSIXlt(Sys.Date())
      } else {
        new.date = as.POSIXlt(project.plan$Startup[ii-1])
      }
      new.date$year = new.date$year + delay.value
      new.date = paste(format(new.date, format ="%Y"), "07-01", sep = "-", collapse = "")
      project.plan$Startup[ii] = new.date
    }
  }
  project.plan$Startup = as.Date(project.plan$Startup)
  save(project.plan, file = "data/project.plan.rda")

  return()
}