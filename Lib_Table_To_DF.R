Lib.TableToDF  <-  function(X,RowHeader){
  
  if(missing(RowHeader)){RowHeader  <-  ""}
  
  
  Y  <-  data.frame(rownames(X))
  names(Y)[1]  <-  RowHeader
  
  ColNames   <-  colnames(X)

  for(i in 1:ncol(X)){
    
    eval(parse(text=paste0("Y$x_", ColNames[i]," <- X[,i]")))
  }
  
  colnames(Y)  <-  c(RowHeader, ColNames)
  
  return(Y)
  
}


######################################################################################################

Lib.TableToMatrix    <-  function(X){
  
  Nrow  <-  nrow(X)
  Ncol  <-  ncol(X)
  
  Y   <-  array(dim = c(Nrow, Ncol), dimnames = dimnames(X))
  
  for(i in 1:Ncol){
    
    Y[,i]  <-  X[,i]
    
  }
  
  return(Y)
  
}