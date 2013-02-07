update.project.stage = function(part = 1) {
  
  usable.columns = names(stage.numbers)[names(stage.numbers) != "Stage.Nr"]
  
  if(part == 1) {
    
    for (ii in 1:nrow(project.plan)) {
      string.to.match = paste(project.plan[ii, usable.columns])
      project.plan$Stage.Nr[ii] = assign.project.stage.number(string.to.match)
    }
    
    stage.numbers.raw = project.plan[names(stage.numbers)]
    stage.numbers.raw = stage.numbers.raw[order(stage.numbers.raw$Project, 
                                                stage.numbers.raw$Company, stage.numbers.raw$Startup),]
    stage.numbers.raw$Stage.Nr[1] = 1
    for (ii in 2:nrow(stage.numbers.raw)) {
      if(all(stage.numbers.raw$Project[ii] == stage.numbers.raw$Project[ii-1] ,
             stage.numbers.raw$Company[ii] == stage.numbers.raw$Company[ii-1])) {
        if (!is.na(stage.numbers.raw$Startup[ii])) {
          stage.numbers.raw$Stage.Nr[ii] = as.numeric(stage.numbers.raw$Stage.Nr[ii-1]) + 1
        }
      } else {
        stage.numbers.raw$Stage.Nr[ii] = 1
      }
    }
       
    write.table(stage.numbers.raw, file ="sources/stage_numbers_raw.txt", sep = "\t", na = "", row.names = FALSE)
  }
  
  if(part == 2) {
    stage.numbers = read.table(file ="sources/stage_numbers_raw.txt", header = TRUE, sep = "\t")
    save(stage.numbers, file = "data/stage.numbers.rda")
    
    project.plan$Startup = as.character(project.plan$Startup)
    for(ii in 1:nrow(project.plan)) {
      project.plan$Stage.Nr[ii] = assign.project.stage.number(paste(project.plan[ii,usable.columns]))
    }
    has.Startup.date = logical(length = nrow(project.plan))
    has.Startup.date[which(as.character(project.plan$Startup) != "")] = TRUE
    project.plan = cbind(project.plan, has.Startup.date)
    save(project.plan, file = "data/project.plan.rda")
  }
  
  
  
}