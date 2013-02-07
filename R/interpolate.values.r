interpolate.values = function(dates, data.vector, type = "production.data") {
  
  datavector.original = data.vector
  n = length(datavector.original)
  
  if(type == "production.data") {
    vector.with.lower.indices = rep.int(NA, n)
    vector.with.lower.indices[!is.na(datavector.original)] = which(!is.na(datavector.original))
    vector.with.lower.indices = fill.from.below(vector.with.lower.indices)
    i.low = vector.with.lower.indices
    
    vector.with.higher.indices = rep.int(NA, n)
    vector.with.higher.indices[!is.na(datavector.original)] = which(!is.na(datavector.original))
    vector.with.higher.indices = fill.from.above(vector.with.higher.indices)
    i.high = vector.with.higher.indices
    
    for (ii in 1:n) {
      numerator = as.integer(dates[ii]) - as.integer(dates[i.low[ii]]) # current value - lower bound
      denominator = as.integer(dates[i.high[ii]]) - as.integer(dates[i.low[ii]]) # higher bound - lower bound
      if (denominator != 0) { 
        date.fraction = numerator / denominator 
      }
      else {
        date.fraction = 0
      }
      production.diff =  datavector.original[i.high[ii]] - datavector.original[i.low[ii]]                                    
      data.vector[ii] = datavector.original[i.low[ii]] + (production.diff * date.fraction) 
    }
  }
  
  if(type == "project.plan") {
    for (ii in 1:n) {
      data.vector[ii] = sum(datavector.original[1:ii] , na.rm = TRUE)
    }
  }
  
  return(data.vector)
} 