standardize.date <-
  function(datestring, format = "auto") {

    datestring = as.character(datestring)
    if (length(grepl("-", datestring)) == 1) {
      datestring = strsplit(datestring, "-")[[1]]
    }
    # will standardize a given date into the standard format.
    # The default format "auto" tests for some known formats
    value = NA
    if (datestring[1] != "") {
      year = datestring[1]
    } else {
      year = NA
    }
    month = "07"
    day = "01"
    if (length(datestring) > 1 ) {
      month = as.integer(datestring[2])
    }
    if (length(datestring) > 2 ) {
      day = as.integer(datestring[3])
    }
    
    if (format == "auto") {
      if (grepl("Start", year)) {
        year = gsub("Start-up: ","",year)
        
      }
      if (grepl("00:00", year)) {
        value = as.Date(year)
        
      }
      if(length(datestring) == 2) {
        if (!is.na(month) && as.integer(month) < 10) {
          month = paste("0", month, sep = "")
        }
        if (is.na(month)) {
          month = "07"
        }
        
      }
    }
    
    if (format == "decimal" || format == "dec") {
      datestring = as.numeric(datestring)
      year = datestring %/% 1
      month = 01
      day = 01
      first.date = as.Date(paste(year, month, day, sep = "-" ))
      days.to.add = ((datestring %% 1) * 365) %/% 1
      value = first.date + days.to.add -1
      year = format(value, "%Y")
      month = format(value, "%m")
      day = format(value, "%d")
      
    }
    
    if(is.na(value)) {
      if (!is.na(year)) {
        value = as.Date(paste(year, month, day, sep = "-" ))
      } else {
        value = NA
      }
    } 
    
    return(value)
  }
