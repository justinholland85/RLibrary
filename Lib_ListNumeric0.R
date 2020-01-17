

Lib.ListNumeric0  <-  function(N){
  
    X  <-  lapply(as.list(rep(1, N)), Lib.Take.Which, -1)
    
  return(X)
  
}

Lib.ListChar0  <-  function(N){
  
  
    X  <-  lapply(as.list(rep("A", N)), Lib.Take.Which, -1)
    
  return(X)
  
}









