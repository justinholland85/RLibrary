Lib.Integer64Fix  <-  function(X){
  
  Class   <-  unlist(lapply(X, class))
  
  N   <-  length(Class)
  
  for(i in 1:N){
    if(Class[i] == "integer64"){X[[i]]  <-  as.numeric(X[[i]])}
    
  }
  
  return(X)
 
}