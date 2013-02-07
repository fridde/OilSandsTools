update.compilation.schemes = function() {
  
  
  new.values = setdiff(names(dataset), Compilation.schemes$Scheme)
  new.values = new.values[new.values != "Dates"]
  
  if(length(new.values) > 0) {
    
    new.rows = data.frame(matrix(data = "", nrow = length(new.values), ncol = ncol(Compilation.schemes)))
    new.rows[,1] = new.values
    names(new.rows) = names(Compilation.schemes)
    
    Compilation.schemes = rbind(Compilation.schemes, new.rows)
    
    write.table(Compilation.schemes, "data/Compilation_schemes.txt", sep = "\t", 
                na = "", quote = FALSE, row.names = FALSE)
    cat(paste("New values added into Compilation_schemes.txt: ", new.values))
    quick.load()    
  }
}