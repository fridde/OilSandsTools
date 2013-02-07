convert.to.barrel.per.day <-
  function (value.vector, unit.def) {
    #browser()
    
    allowed.units = c("Thousands of cubic metres per year", "Thousands of cubic metres per month"  ,           
                      "Thousands of barrels bitumen per day", "Thousand Cubic Metres per year" ,                 
                      "Million barrels per day", "Production (m3) - Bitumen (per year)",          
                      "Production (m3) - Synthetic Crude Oil (per year)", "Barrels of Bitumen per day", 
                      "Barrels of bitumen per day", "Thousand Cubic meters per day","Thousand barrels per day", 
                      "thousand cubic metres per day")
    if(length(unit.def) > 0 && !any(grepl(unit.def[1], allowed.units, fixed = TRUE))) {
      cat(paste("The unit - ", unit.def, " - was not found", collapse = ""))
      return(paste("The unit - ", unit.def, " - was not found", collapse = ""))
    }
    
    return.vector = c()
    if (length(value.vector) > 0) {
      for (ii in 1:length(value.vector)) {
        
        value = as.numeric(gsub(",","",value.vector[ii]))
        recognized = FALSE
        
        if (unit.def[ii] == "Production (m3) - Bitumen (per year)") {
          value = value * (1/0.158987294928) * (1/365)  #  * (barrels / m3) * (years/day)
          recognized = TRUE
        }
        if (unit.def[ii] == "Production (m3) - Synthetic Crude Oil (per year)") {
          value = value * (1/0.158987294928) * (1/365) * (1/0.86)  # (barrels / m3) * (years/day) (bitumen / SCO) ,taken from [Mejean, Hope, 2010 , p.22]
          recognized = TRUE
        }
        if (any(unit.def[ii] == c("Barrels of Bitumen per day", "Barrels of bitumen per day")) ) {
          value = value
          recognized = TRUE
        }
        if (any(unit.def[ii] == c("Thousands of m3 bitumen per day", "Thousand Cubic meters per day",
                                  "thousand cubic metres per day")) ) {
          value = value * 1000 *  (1/0.158987294928)  # 1000 * (barrels / m3)
          recognized = TRUE
        }
        if (any(unit.def[ii] == c("Thousand Cubic Metres per year", "Thousands of cubic metres per year")) ) {
          value = value * 1000 * (1/365) * (1/0.158987294928) # 1000 * (years/day) * (barrels/m3) 
          recognized = TRUE
        }
        if (any(unit.def[ii] == c("Thousand barrels per day", "Thousands of barrels bitumen per day")) ){
          value = value * 1000
          recognized = TRUE
        }
        if (any(unit.def[ii] == c("Million barrels per day")) ){
          value = value * 1000000 
          recognized = TRUE
        }
        if (any(unit.def[ii] == c("Thousands of cubic metres per month")) ){
          value = value * 1000 * (12/365) * (1/0.158987294928) # 1000 * (months/(day/year)) * (barrels/m3)
          recognized = TRUE
        }

        
        if (recognized) {
          return.vector = c(return.vector, round(value))
        } else {
          return.vector = c(return.vector, "Unit definition unknown")
        }
      }
    }
    return(return.vector)
  }
