
Lib.AsNumeric  <-  function(X){
  
  if(class(X) %in% c("integer", "integer64")){
    
    X  <-  as.numeric(X)
    
  }
  
  return(X)
  
}