filter.dataframe = function(dataframe, scheme) {

  if (length(scheme) != 1) {
    paste("Too few, too many or non-existent schemes provided!")
  } 
  
  columns.to.choose = Compilation.schemes$Scheme[which(Compilation.schemes[,scheme] == "x")]
  years = Compilation.schemes$Year[which(Compilation.schemes[,scheme] == "x")]
  columns.to.choose = columns.to.choose[order(years)]
  column.names = character()
  for (current.column in columns.to.choose) {
    current.short.name = Compilation.schemes$Short.Name[Compilation.schemes$Scheme == current.column]
    if (current.short.name == "") {
      current.short.name = Compilation.schemes$Scheme[Compilation.schemes$Scheme == current.column]
    }
    column.names = c(column.names, current.short.name)
  }
  
  return.dataframe = dataframe[c("Dates", columns.to.choose)]
  names(return.dataframe) = c("Dates", column.names)
  return(return.dataframe)
}