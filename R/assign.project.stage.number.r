assign.project.stage.number = function(string.to.match) {
  
  usable.columns = names(stage.numbers)[names(stage.numbers) != "Stage.Nr"]
  string.to.match[string.to.match == "NA"] = "" 
  
  matching.rows = numeric()
  for (ii in 1:nrow(stage.numbers)) {
    if (all(string.to.match == paste(stage.numbers[ii, usable.columns]))) {
      matching.rows = c(matching.rows, ii)
    }
  }
  
  if(length(matching.rows) == 0) {
    
    stage.number = ""
  }
  if (length(matching.rows) == 1) {
    stage.number = stage.numbers$Stage.Nr[matching.rows]
  } 
  if (length(matching.rows) > 1) {
    stage.number = "Too many possibilities. Check your algorithm!"
  }
  
  return(stage.number)
}