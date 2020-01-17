Lib.Tuple.Match   <-  function(X, Index){
  

  Ncol          <-  ncol(Index)
  
  
  Names.X       <-  paste0("X_", seq(1,Ncol))
  Names.Y       <-  paste0("X_", seq(1,Ncol))
  
  names(X)      <-  Names.X
  names(Index)  <-  Names.Y
  
  
  Index.Y       <-  seq(1, nrow(Index))  
  Index.X       <-  seq(1, nrow(X))  
  
  DT.X          <-  data.table(X, Index.X)
  
  DT.Y          <-  data.table(Index, Index.Y)
  
  
  Merged        <-  merge(DT.X, DT.Y, sort=FALSE,
                           by.x = Names.X, by.y =  Names.Y)
  
  Match         <-  Merged$Index.Y[match(Index.X, Merged$Index.X)]
  
  
  return(Match)
  
}






