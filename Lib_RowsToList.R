
Lib.RowsToList  <-  function(X){

  Y   <-  apply(X, 1, list)
  Y    <-  lapply(  Y , unlist)
    return(Y)
}





