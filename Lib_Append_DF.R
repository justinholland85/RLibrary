Lib.Append.DF  <-  function(X, Y){
  
  
  Names.X                       <-  names(X)
  Names.Y                       <-  names(Y)
  
  Names.A                       <-  union(Names.X,Names.Y)
  
  X[setdiff(Names.A, Names.X)]  <-  NA
  Y[setdiff(Names.A, Names.Y)]  <-  NA
  
  X                             <-  X[,Names.A]   
  Y                             <-  Y[,Names.A]   
  
  Appended                      <-  Lib.Rbind.DF(X, Y)
  
  return(Appended)
  
}


