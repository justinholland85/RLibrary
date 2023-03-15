Lib.Integer64Fix  <-  function(X){
  

  for(i in 1:ncol(X)){
    if(class(X[[i]] == "integer64")){X[[i]]  <-  as.numeric(X[[i]])}
    
  }
  
  return(X)
 
}