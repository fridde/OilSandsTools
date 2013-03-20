select.historic <-
  function(date.value.vector, col.to.check = NA, compilation = "") {
    
    # col.to.check should be given as an atomic string character in the form "column.name1 == value.A, column.name2 >= value.B, ...") 
    # The elements of col.to.check will be evaluated as expressions.
    # If literal is TRUE, Project- and Company-names will not be converted to their main-names
    # valid column names are  given below:
    # "Project", "Company", "Combined.Project.Name", "Year", "Month", "Value", "Unit.Def", "Data.Source"
    
    # Value: The sum of all production for the given date. Unit: Barrels of bitumen per day
    
    data.table.type = attribution.table$Data.Table.Type[min(which(attribution.table$Main.Compilation == compilation), na.rm = TRUE)]      
    
    if (!is.na(col.to.check)) {
      col.to.check = strsplit(col.to.check, ",")[[1]]
    }
    
    compilation.indices = which(production.data$Main.Compilation == compilation)
    production.values.vector = c()
    for (i.date in 1:length(date.value.vector)) {
      
      indices = compilation.indices
      if (!is.na(col.to.check)) {
        for (ii in 1:length(col.to.check)) {
          
          current.expression = strsplit(col.to.check[ii]," ")[[1]]
          current.expression = gsub("^[ \t]+|[ \t]+$", "", current.expression) #remove trailing or ending space
          if (current.expression[2] == "==" | current.expression[2] == "=") {
            matching.indices = which(production.data[current.expression[1]] == paste(current.expression[3:length(current.expression)], collapse = " "))
            indices = intersect(indices, matching.indices)
          }
          if (current.expression[2] == "!=") {
            matching.indices = which(production.data[current.expression[1]] != paste(current.expression[3:length(current.expression)], collapse = " "))
            indices = intersect(indices, matching.indices)
          }
          if (current.expression[2] == "<=") {
            matching.indices = which(production.data[current.expression[1]] <= as.numeric(paste(current.expression[3:length(current.expression)], collapse = " ")))
            indices = intersect(indices, matching.indices)    
          }
          if (current.expression[2] == ">=") {
            matching.indices = which(production.data[current.expression[1]] >= as.numeric(paste(current.expression[3:length(current.expression)], collapse = " ")))
            indices = intersect(indices, matching.indices)
          }
          if (current.expression[2] == ">") {
            matching.indices = which(production.data[current.expression[1]] > as.numeric(paste(current.expression[3:length(current.expression)], collapse = " ")))
            indices = intersect(indices, matching.indices)
          }
          if (current.expression[2] == "<") {
            matching.indices = which(production.data[current.expression[1]] < as.numeric(paste(current.expression[3:length(current.expression)], collapse = " ")))
            indices = intersect(indices, matching.indices)
          } 
        }
      }
      
      indices = intersect(indices, which(production.data$Dates == date.value.vector[i.date]))
      indices = intersect(indices, compilation.indices)
      Values = convert.to.barrel.per.day(production.data$Value[indices], production.data$Unit.Def[indices])
      production.values.vector[i.date] = sum(Values, na.rm = TRUE)
    }
    production.values.vector[production.values.vector == "0"] = NA
    production.values.vector = interpolate.values(date.value.vector, production.values.vector, type = data.table.type)
    
    return(production.values.vector)
  }