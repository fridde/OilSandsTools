manual.input = function(files.to.update) {
  
  setwd("sources")
  files.to.update = paste(files.to.update, ".txt", sep = "")
  
  for (ii in 1:length(files.to.update)) {
   is.updated = FALSE
    while(!is.updated) {
      if (length(list.files(pattern = files.to.update[ii])) == 0) {
        write(c(), file = files.to.update[ii])
      }
      shell.exec(files.to.update[ii])
      is.updated = grepl("yes", varEntryDialog(vars = "Write Yes", title = paste("Did you update and save", 
                                                                                 files.to.update[ii], "?"))[[1]], 
                         ignore.case = TRUE)
    }
    
  }
  setwd("..")
}