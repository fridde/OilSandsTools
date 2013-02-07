select.prognosis <-
  function(date.value.vector, col.to.check = NA, literal = FALSE , compilation = "") {
    # col.to.check should be given as an atomic string character in the form "column.name1 == value.A, column.name2 >= value.B, ...") 
    # The elements of col.to.check will be evaluated as expressions.
    # valid column names are  given below: 
    # "Project", "Company", "Stage", "Type", "Costs", "Operating.Status", "AEUB.Status", "Startup", "Value", "Unit.Def", 
    # "Description", "Descr.Updated", "Year.of.Prognosis", "Data.Source"
  
    data.table.type = attribution.table$Data.Table.Type[min(which(attribution.table$Main.Compilation == compilation), na.rm = TRUE)]
    
    
    if (data.table.type == "project.plan") {
      
      compilation.indices = which(project.plan$Main.Compilation == compilation)
      production.values.vector = c()
      for (i.date in 1:length(date.value.vector)) {
        
        indices = compilation.indices
        if (!is.na(col.to.check)) {
          for (ii in 1:length(col.to.check)) {
            
            current.expression = strsplit(col.to.check[ii]," ")[[1]]
            current.expression = gsub("^[ \t]+|[ \t]+$", "", current.expression) #remove trailing or ending space
            if (current.expression[2] == "==" | current.expression[2] == "=") {
              matching.indices = which(project.plan[current.expression[1]] == paste(current.expression[3:length(current.expression)], collapse = " "))
              indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == "!=") {
              matching.indices = which(project.plan[current.expression[1]] != paste(current.expression[3:length(current.expression)], collapse = " "))
              indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == "<=") {
              matching.indices = which(project.plan[current.expression[1]] <= as.numeric(paste(current.expression[3:length(current.expression)], collapse = " ")))
              indices = intersect(indices, matching.indices)    
            }
            if (current.expression[2] == ">=") {
              matching.indices = which(project.plan[current.expression[1]] >= as.numeric(paste(current.expression[3:length(current.expression)], collapse = " ")))
              indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == ">") {
              matching.indices = which(project.plan[current.expression[1]] > as.numeric(paste(current.expression[3:length(current.expression)], collapse = " ")))
              indices = intersect(indices, matching.indices)
            }
            if (current.expression[2] == "<") {
              matching.indices = which(project.plan[current.expression[1]] < as.numeric(paste(current.expression[3:length(current.expression)], collapse = " ")))
              indices = intersect(indices, matching.indices)
            } 
          }
        }
        
        indices = intersect(indices, which(project.plan$Startup == date.value.vector[i.date]))
        indices = intersect(indices, compilation.indices)
        
        Values = convert.to.barrel.per.day(project.plan$Value[indices], project.plan$Unit.Def[indices])
        production.values.vector[i.date] = sum(Values)
      }
      production.values.vector[production.values.vector == "0"] = NA
      production.values.vector = interpolate.values(date.value.vector, production.values.vector, type = data.table.type)
    }
    
    if (data.table.type == "production.data") {
      production.values.vector = select.historic(date.value.vector, compilation = compilation)
    }
    
    
    return(production.values.vector)
  }
