combine.curves = function(dataset) {
  
  new.dataset = dataset
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("CAPP Mining 2006", "CAPP In-Situ 2006")] ,na.rm = TRUE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CAPP 2006"
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("CAPP Mining Prognosis 2012", "CAPP In-Situ Prognosis 2012")] ,na.rm = TRUE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CAPP Prognosis 2012"
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("CAPP Mining SCO 2012", "CAPP In-situ SCO 2012")] ,na.rm = TRUE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CAPP SCO 2012"
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("CAPP Mining Historical 2012", "CAPP In-Situ Historical 2012")] ,na.rm = TRUE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CAPP Bitumen Historical 2012"
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("CAPP Mining Historical 2012", "CAPP In-Situ Historical 2012", 
                                 "CAPP Mining SCO 2012", "CAPP In-situ SCO 2012")] ,na.rm = TRUE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CAPP Historical 2012"
  #---------------------------------------------------------------------------------------
  vector.to.subtract = (-1) * dataset["AEUB In-Situ 2005"]
  new.vector = rowSums(data.frame(dataset["AEUB Mining+In-Situ 2005"], vector.to.subtract) ,na.rm = FALSE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "AEUB Mining 2005"
  #---------------------------------------------------------------------------------------
  new.vector = dataset["AEUB Mining+In-Situ 2005"]
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "AEUB 2005"
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("Canada's Energy Future - In-Situ, Supply Push 2003",
                                 "Canada's Energy Future - Mining, Supply Push 2003")] ,na.rm = FALSE) 
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CEF - Supply Push 2003"
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("Canada's Energy Future - Mining, Techno-Vert 2003",
                                 "Canada's Energy Future - In-Situ, Techno-Vert 2003")] ,na.rm = FALSE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CEF - Techno-Vert 2003"
  #---------------------------------------------------------------------------------------
  vector.to.subtract = (-1) * dataset["Canada's Energy Future - Upgraded 2007"]
  new.vector = rowSums(data.frame(dataset["Canada's Energy Future - Upgraded+Non-Upgraded 2007"], vector.to.subtract) ,na.rm = TRUE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CEF Non-Upgraded 2007"
  #---------------------------------------------------------------------------------------
  new.vector = dataset["Canada's Energy Future - Upgraded 2007"]
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CEF Upgraded 2007"
  #---------------------------------------------------------------------------------------
  vector.to.subtract = (-1) * dataset["NEB Upgraded 2009"]
  new.vector = rowSums(data.frame(dataset["NEB Upgraded+Non-Upgraded 2009"], vector.to.subtract) , na.rm = FALSE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "NEB Non-Upgraded 2009" 
  #---------------------------------------------------------------------------------------
  vector.to.subtract = (-1) * dataset["Citibank - All Oil minus Oil Sands 2012"]
  new.vector = rowSums(data.frame(dataset["Citibank - All Oil 2012"], vector.to.subtract) , na.rm = FALSE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "Citibank 2012"
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("NEB In-Situ $14, 2000", "NEB Mining $14, 2000")] ,na.rm = FALSE) 
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "NEB $14, 2000"
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("NEB In-Situ $18, 2000", "NEB Mining $18, 2000")] ,na.rm = FALSE) 
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "NEB $18, 2000"
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("NEB In-Situ $22, 2000", "NEB Mining $22, 2000")] ,na.rm = FALSE) 
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "NEB $22, 2000"
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("Statistics Canada - Crude Bitumen 2012",
                                 "Statistics Canada - SCO 2012")] ,na.rm = FALSE) 
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "StatCan Bitumen + SCO 2012"
  #---------------------------------------------------------------------------------------
  vector.to.subtract = (-1) * dataset["CERI Awaiting Approval+Suspended+Approved+Construction+Onstream 2011"]
  new.vector = rowSums(data.frame(dataset["CERI Announced+Awaiting Approval+Suspended+Approved+Construction+Onstream 2011"],
                                  vector.to.subtract) , na.rm = TRUE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CERI Announced 2011"
  #---------------------------------------------------------------------------------------
  vector.to.subtract = (-1) * dataset["CERI Suspended+Approved+Construction+Onstream 2011"]
  new.vector = rowSums(data.frame(dataset["CERI Awaiting Approval+Suspended+Approved+Construction+Onstream 2011"],
                                  vector.to.subtract) , na.rm = TRUE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CERI Awaiting Approval 2011"
  #---------------------------------------------------------------------------------------
  vector.to.subtract = (-1) * dataset["CERI Approved+Construction+Onstream 2011"]
  new.vector = rowSums(data.frame(dataset["CERI Suspended+Approved+Construction+Onstream 2011"],
                                  vector.to.subtract) , na.rm = TRUE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CERI Suspended 2011"
  #---------------------------------------------------------------------------------------
  vector.to.subtract = (-1) * dataset["CERI Construction+Onstream 2011"]
  new.vector = rowSums(data.frame(dataset["CERI Approved+Construction+Onstream 2011"],
                                  vector.to.subtract) , na.rm = TRUE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CERI Approved 2011"
  #---------------------------------------------------------------------------------------
  vector.to.subtract = (-1) * dataset["CERI Onstream 2011"]
  new.vector = rowSums(data.frame(dataset["CERI Construction+Onstream 2011"],
                                  vector.to.subtract) , na.rm = TRUE)
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "CERI Construction 2011"
  #---------------------------------------------------------------------------------------
  new.vector = rowSums(dataset[c("NEB Mining 2011",
                                 "NEB In-Situ 2011")] ,na.rm = FALSE) 
  new.vector[new.vector == 0] = NA
  new.dataset = cbind (new.dataset, new.vector)
  names(new.dataset)[ncol(new.dataset)] = "NEB 2011"
  #---------------------------------------------------------------------------------------
  
  
  
  
  
  return(new.dataset)
}
