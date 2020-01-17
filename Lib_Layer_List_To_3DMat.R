Lib.Layer.List.To.3DMat  <-  function(X){
  
  N  <- length(X)
  DimNames   <-  list(names(X), rownames(X[[1]]), colnames(X[[1]]))
  Y  <-  array(dim=c(N,dim(X[[1]])), dimnames = DimNames)
  
  for(i in 1:N){
    Y[i,,]  <-  X[[i]]
  }
  
 return(Y) 
  
}


