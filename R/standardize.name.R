standardize.name <-
  function(alias.name, type = "") {
    # replaces a project- or company name with its standardized name
    type = rep.int(type, length(alias.name))
    return.vector = c()
    
    for (ii in 1:length(alias.name)) {
      if (type[ii] == "") {
        value = synonym.names$Main.Name[synonym.names$Alias.Name == alias.name[ii]]
      }
      else {
        value1 = synonym.names$Main.Name[synonym.names$Alias.Name == alias.name[ii]]
        value2 = synonym.names$Main.Name[synonym.names$Type == type[ii]]
        value = intersect(value1, value2)
      }
      if (length(value) == 1) {
        return.vector = c(return.vector, value)
      }
      if (length(value) > 1) {
        cat("Could not replace value", alias.name[ii], ". Check synonym.names. 
        Your value should exist exactly once in the column Alias.Name or a type has to be specified!")
        return.vector = c(return.vector, NA)
      }
      if (length(value) < 1) {
        if (alias.name[ii] == "") {
          return.vector = c(return.vector, "")
        }
        else {
          return.vector = c(return.vector, NA)
        }
      }
    }
    return(return.vector)
  }
