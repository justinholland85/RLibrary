Lib.DF.Unlist   <-  function(DF){
  
  VarNames      <-  names(DF)

  InnerCode     <-  paste(paste0(paste0("DF$"), VarNames, ", "), collapse ="")
  
  Code          <-  paste0("X <- c(", c(InnerCode), "NULL)")
  
  return(X)
  
}