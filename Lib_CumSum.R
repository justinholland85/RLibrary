Lib.CumSum  <-  function(X){
  
  
  Y      <-  array(dim = dim(X))
  Y[,1]  <-  X[,1]
  
  for(i in 2:ncol(X)){
    
  Y[,i]  <-  Y[,i-1] + X[,i]
    
  }
  
  return(Y)  

  
}

######################################################################################################
Lib.ProbMat <-  function(X, u){
  
  Nrow       <-  nrow(X)
  Ncol       <-  ncol(X)
  
  if(missing(u)){ u <-  runif(Nrow)}
  
  
  Y   <-  1 * (0 < u & u <= X[,1])
  
  for(i in 2:Ncol){
    
    Y  <- Y + i *  (X[,i-1] < u & u <= X[,i])
    
  }
  
  return(Y)
  
  
}