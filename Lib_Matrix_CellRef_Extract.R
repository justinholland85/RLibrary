Lib.Matrix.CellRef.Extract  <-  function(Row, Column, Table){
  
  
  Nrow  <-  nrow(Table)

  Ref   <-  (Column - 1) * Nrow + Row
  
  Y  <-  Table[Ref]
  
  return(Y)
  
  
}