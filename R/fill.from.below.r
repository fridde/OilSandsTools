fill.from.below = function (data.vector , replace = NA, initial = 1) {
  # fills a vector with NA-values with the closest value from above that is usable 
  
  if (length(data.vector) > 0) {
    if (!is.na(replace)) {
      data.vector[data.vector == replace] = NA
    }
    
    return.vector = data.vector
    if (is.na(return.vector[1])) {
      return.vector[1] = initial
    }
    
    for (ii in 2:length(return.vector)) {
      if (is.na(return.vector[ii])) {
        return.vector[ii] = return.vector[ii-1]
      }
    }
    return(return.vector)
  }
}