load.all.databases = function() {
  
  rda.file.list = list.files(paste(main.dir, "data/", sep = ""), pattern = ".rda")
  for (rda.file in rda.file.list) {
    load(paste(main.dir, "data/", rda.file, sep = ""), .GlobalEnv)
  }
  
  txt.file.list = list.files(paste(main.dir, "data/", sep = ""), pattern = ".txt")
  for (txt.file in txt.file.list) {
    new.table =  read.delim(paste(main.dir, "data/", txt.file, sep = ""))
    table.name = gsub("_",".", txt.file)
    table.name = gsub(".txt","", table.name)
    assign(table.name, new.table, envir = .GlobalEnv)    
  }
  
  
}