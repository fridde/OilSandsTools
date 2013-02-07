standardize.date.list = function(datestring.matrix, format = "auto") {

  datestring.matrix = as.matrix(datestring.matrix)
  if (length(format) == 1) {
    format = rep.int(format, nrow(datestring.matrix))
  }
  return.vector = c.Date()
  for (ii in 1:nrow(datestring.matrix)) {
    return.vector = c(return.vector, standardize.date(paste(datestring.matrix[ii,]) , format = format[ii]))
  }

  return(return.vector)
}