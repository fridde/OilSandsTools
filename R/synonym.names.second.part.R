synonym.names.second.part <-
  function() {
   
    setwd("sources")
    syn.tab = read.table("synonym_names_raw_reconciled.txt", sep= "\t", as.is = TRUE, header = TRUE)
    setwd("..")
    
    Main.Name = rbind(as.matrix(syn.tab$Main.Project.Name), as.matrix(syn.tab$Main.Company.Name))
    Alias.Name = rbind(as.matrix(syn.tab$Synonym.Project.Name), as.matrix(syn.tab$Synonym.Company.Name))
    Type = rbind(as.matrix(syn.tab$Project.Type), as.matrix(syn.tab$Company.Type))
    
    synonym.names = data.frame(Main.Name, Alias.Name, Type, stringsAsFactors = FALSE, row.names = NULL)
    synonym.names = synonym.names[synonym.names$Main.Name != "",]
    synonym.names = unique(synonym.names)
    synonym.names = synonym.names[order(synonym.names$Type, synonym.names$Main.Name),]
    
    setwd("data")
    save(synonym.names, file = "synonym.names.rda")
    setwd("..")
    
  }
