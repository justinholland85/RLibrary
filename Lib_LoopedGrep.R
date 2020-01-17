
Lib.LoopedGrepValues   <-  function(X,Pattern){
  
  Y  <-  character(0)
  N  <-  length(Pattern)
  
  for(i in 1:N){
    
    Y  <- sort(unique(union(Y, grep(X, pattern = Pattern[i], value = TRUE))))
    
  }
  
  return(Y)
  
}
