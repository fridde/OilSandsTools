fill.from.above = function (data.vector , replace = NA, initial = length(data.vector)) {
  # fills a vector with NA-values with the closest value from above that is usable 
  # "above" here means NOT the graphical "above", but a higher row-index
  if (length(data.vector) > 0) {
    if (!is.na(replace)) {
      data.vector[data.vector == replace] = NA
    }
    
    return.vector = data.vector
    if (is.na(return.vector[length(return.vector)])) {
      return.vector[length(return.vector)] = initial
    }
    
    for (ii in (length(return.vector)-1):1) {
      if (is.na(return.vector[ii])) {
        return.vector[ii] = return.vector[ii+1]
      }
    }
    return(return.vector)
  }
}