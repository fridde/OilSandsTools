time.filter <-
function (dataframe, start.year= "1900", end.year = "2499", start.day = "1900-01-01", end.day = "2500-01-01") {
  # filters a date.vector so that it only contains values between start.time and end.time
  
  if (start.year != "1900") {start.day = paste(start.year, "-01-01", sep ="") }
  if (end.year != "2499") {end.day = paste(end.year, "-12-31", sep ="") }
  
  date.vector = as.Date(dataframe[["Dates"]])
  start.day = as.Date(start.day)
  end.day = as.Date(end.day)
  
  before.end = which(as.integer(date.vector) <= as.integer(end.day))
  after.start = which(as.integer(date.vector) >= as.integer(start.day))
  
  return.frame = dataframe[intersect(before.end, after.start),]
    
  return(return.frame)
}
