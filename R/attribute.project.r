attribute.project = function(project.values.string, columns.to.include = "") {
  
  if (length(columns.to.include) == 1 && columns.to.include == "") {
    columns.to.include = names(attribution.table)
    columns.to.include = columns.to.include[columns.to.include != "Main.Compilation"]
  }
  values.to.test = rep.int(FALSE, nrow(attribution.table))
  
  for (ii in 1:nrow(attribution.table)) {
    values.to.test[ii] = all(project.values.string == paste(attribution.table[ii,columns.to.include]))
  }
  
  if(length(which(values.to.test)) == 1) {
    Main.Compilation = attribution.table$Main.Compilation[which(values.to.test)]
  } else {
    Main.Compilation = NA
  }
  return(Main.Compilation)
}
