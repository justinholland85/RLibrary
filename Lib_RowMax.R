Lib.RowMax    <-  function(X){
  
# This function is equivalent to apply(X, 1, max, na.rm=TRUE) but much faster
  
    Max   <-  rep(-Inf, nrow(X))
  
    for(i in 1:ncol(X)){
   
    Max   <-  pmax(Max, X[,i], na.rm=TRUE)
  
    }
  
  return(Max)
    
}

######################################################################################################


Lib.RowMin    <-  function(X){
  
  # This function is equivalent to apply(X, 1, min, na.rm=TRUE) but much faster
  
  Min   <-  rep(Inf, nrow(X))
  
  for(i in 1:ncol(X)){
    
    Min   <-  pmax(Min, X[,i], na.rm=TRUE)
    
  }
  
  return(Min)
  
}