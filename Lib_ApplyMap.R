

Lib.ApplyMap  <-  function(X, MapFrom, MapTo, Pass){
  
  if(missing(Pass)){Pass  <- 1}
  
  Y  <- MapTo[match(X, MapFrom)]
  
  if(Pass == 1){
    
    Which   <-  which(is.na(Y))
    
    Y[Which]  <-  X[Which]    
    
    
  }
  
  return(Y)
  
}







