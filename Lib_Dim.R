Lib.Dim  <-  function(X){
  
  Dim    <-  dim(X)
  if(is.null(Dim)){Dim  <- length(X)}
  
  return(Dim)
  
}