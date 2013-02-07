compile.prod.proj <-
  function(hist.compilations = "", prog.compilations = "", col.to.check = NA, start.year = "1900", end.year = "2499") {
    
    add.unknown.date()
    quick.load()
    date.column = extract.all.dates()
    
    return.frame = data.frame(Dates = date.column)
    
    if(hist.compilations == "") {
      hist.compilations = unique(attribution.table$Main.Compilation[attribution.table$Data.Type == "Historical"])
    }
    
    for (i.compilation in hist.compilations) {
      cat(i.compilation, " ### ")
      return.frame = cbind(return.frame, select.historic(date.value.vector = date.column, compilation = i.compilation))
      names(return.frame)[ncol(return.frame)] = i.compilation
    }
    
    if (prog.compilations == "") {
      prog.compilations = unique(attribution.table$Main.Compilation[attribution.table$Data.Type == "Prognosis"])
    }
    
    for (i.compilation in prog.compilations) {
      #if (i.compilation == "NEB Mining 2011") {browser()}
      cat(i.compilation, " ### ")
      return.frame = cbind(return.frame, select.prognosis(date.value.vector = date.column, compilation = i.compilation))
      names(return.frame)[ncol(return.frame)] = i.compilation
    }
    
    return.frame = combine.curves(return.frame)
    
    Lower.05 = rep.int(NA, nrow(return.frame))
    Middle = rep.int(NA, nrow(return.frame))
    Upper.95 = rep.int(NA, nrow(return.frame))
    
    return.frame = cbind(return.frame, Lower.05,  Middle, Upper.95)
    Recent.Prognoses = Compilation.schemes$Scheme[which(Compilation.schemes$Recent == "x")]
    for (ii in 1:nrow(return.frame)) {
      boundaries = quantile(return.frame[ii,Recent.Prognoses], probs = c(0.1, 0.5, 0.9), na.rm = TRUE) 
      return.frame$Lower.05[ii] =  boundaries[[1]]
      return.frame$Middle[ii] = boundaries[[2]]
      return.frame$Upper.95[ii] = boundaries[[3]]
    }
    
    update.compilation.schemes()
    
    return(return.frame)
  }
