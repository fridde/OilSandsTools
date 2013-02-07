calculate.correlations = function(base.function = NA) {
  
  if (is.na(base.function)) {
    base.functions = Compilation.schemes$Scheme[which(Compilation.schemes[,"Type"] == "Historical")]
  }
  correlation.data = subset(correlation.data, correlation.data$List.Nr == "+++") # clear the table
  list.of.lm = list()
  
  for (base.function in base.functions) {
  
    columns.to.exclude = union(which(names(dataset) == "Dates"), which(names(dataset) == base.function))
    columns = names(dataset)[-columns.to.exclude]
    
    for (ii in 1:length(columns)) {
    
      column = columns[ii]
      #if (column == "WEO 2012") {browser()}
      cat(paste("Calculating correlation between ", base.function, " and ", column), fill = TRUE)
      column.source.date = paste(Compilation.schemes$Year[Compilation.schemes$Scheme == column], "01-01" ,
                                 sep = "-", collapse = "")
      dataframe = dataset[c(base.function, column)]
      indices = c()
      for (iii in 1:nrow(dataset)) {
        first = !is.na(dataframe[iii, base.function])
        second = !is.na(dataframe[iii, column])
        is.after.time = standardize.date(column.source.date) <= dataset[iii,"Dates"]
        if (all(first, second, is.after.time)) {
          indices = c(indices, iii)
        }
      }
      if (length(indices) > 2) {
        dataframe = dataframe[indices,]
        x = dataframe[[column]]
        y = dataframe[[base.function]]
        if(!is.na(cor(x,y))) {
          lm.model = lm(y ~ x)
          lm.summary = summary.lm(lm.model)
          
          Curve.Name = column
          Intercept = lm.model$coefficients[[1]]
          Slope = lm.model$coefficients[[2]]
          Correlation = cor(x, y)
          p.Value = pf(lm.summary$fstatistic[1],lm.summary$fstatistic[2],
                       lm.summary$fstatistic[3], lower.tail = FALSE)
          Time.Frame = dataset$Dates[indices[length(indices)]] - dataset$Dates[indices[1]]
          Time.Frame = paste(Time.Frame, "days", collapse = " ")
          
          List.Nr = length(list.of.lm) + 1
          
          correlation.data.names = names(correlation.data)
          correlation.data = rbind(correlation.data, c(Curve.Name, Intercept, Slope, Correlation, 
                                                       p.Value, base.function, Time.Frame, List.Nr))
          names(correlation.data) = correlation.data.names
         
          list.of.lm[[List.Nr]] = lm.model
        }
             
      }
    }
  }
  correlation.data = unique(correlation.data)
  correlation.data = correlation.data[order(correlation.data$Correlation, decreasing = TRUE),]
  #list.of.lm = unique(list.of.lm)
  save(correlation.data, file = "data/correlation.data.rda")
  save(list.of.lm, file = "data/list.of.lm.rda")
  
}