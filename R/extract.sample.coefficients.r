extract.sample.coefficients = function(data.set) {

#   # first put together factors to search for
#   Exact.Types = paste("Type =", unique(project.plan$Type))
#   Common.Types = paste("Exact.Type =", unique(conglomerate.types(Exact.Types)))
#   Projects = paste("Project =", unique(synonym.names$Main.Name[synonym.names$Type == "Project"]))
#   Companies = paste("Company =", unique(synonym.names$Main.Name[synonym.names$Type == "Company"]))
#   
#   Larger.Dates = paste("Dates >", as.character(all.the.dates))
#   Smaller.Dates = paste("Dates <", as.character(all.the.dates))
#   
#   test.for.vector = c(Exact.Types, Projects,  Companies)
#   # test.for.vector = c(Exact.Types, Common.Types, Projects,  Companies, Larger.Dates, Smaller.Dates)
#   
#   sample.frame = data.frame(matrix(ncol = ncol(data.set), nrow = 0))
#   names(sample.frame) = names(data.set)
#   Test.Value = character()
#   sample.frame = cbind(Test.Value, sample.frame)
#   
#   for (i.test.for.vector in test.for.vector){
#     
#     current.frame = compile.prod.proj(col.to.check = i.test.for.vector)
#     current.frame = interpolate.values(current.frame)
#     test.value = rep.int(i.test.for.vector, nrow(current.frame))
#     current.frame = cbind(test.value, current.frame)
#       
#     sample.frame = rbind(sample.frame, current.frame)
#   }
#   
#   prog.columns = grep("Plan", names(sample.frame), ignore.case = TRUE)
#   # calculate parameters needed for simulation
#   for (i.row in 1:nrow(sample.frame)) {
#     if (!is.na(sample.frame$Production[i.row]) && any(sample.frame[i.row, prog.columns] != 0)){
#       
#         
#       ## continue here... 121023
#     }
#   }

  prog.columns = grep("Plan", names(data.set), ignore.case = TRUE)
  nr.prog.columns = length(prog.columns)
    
  for (i.prog.col in prog.columns) {
    quota = data.set$Production / data.set[i.prog.col]
    data.set = cbind(data.set, quota)
    names(data.set)[ncol(data.set)] = paste("Quota", names(data.set[i.prog.col]) , sep = ".")
  }
  
  return.data.set = data.set
  
  return(return.data.set)
}