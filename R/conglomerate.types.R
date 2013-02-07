conglomerate.types <-
  function (Type.vector) {
    #browser(text="conglomerate.types")
    # replaces the type of a project with a more common type to enable clustering
    
    alias.names.in.situ = c("SAGD", "TAGD", "ET-DSP", "In Situ", "CSS", "SLP-SAGD", "SAP-SAGD", "Air injection", "SC-SAGD", "Bitumen Extraction Solvent Techn", 
                            "LP-SAGD", "CSS & SAGD", "THAI", "Vertical Steam Drive")
    alias.names.mining = c("Surface Mining")
    
    return.vector = c()
    for (ii in 1:length(Type.vector)) {
      
      if (any(alias.names.in.situ == Type.vector[ii])) {
        Type = "In Situ"
        if (any(alias.names.mining == Type.vector[ii])) {
          Type = "Mining"
        }
      } else {
        Type = Type.vector[ii]
      }
      
      return.vector = c(return.vector, Type)
    }
    
    return(return.vector)
  }
