extract.all.dates <- function(){
  
  value = as.Date(c(project.plan$Startup, production.data$Dates))
  value = sort(unique(value))
  
  return (value)
  
}
