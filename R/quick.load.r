quick.load = function () {
  
  options(stringsAsFactors = FALSE)
  library("stringr")
  library("gplots")
  functions.to.load = list.files(paste(main.dir, "R/", sep = ""))
  
  for (ii in 1:length(functions.to.load)){
    source(paste(main.dir, "R/", functions.to.load[ii], sep = ""), local = FALSE)    
  }
  load.all.databases()
}