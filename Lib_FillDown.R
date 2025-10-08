Lib.FillDown  <-  function(X, Y){
  
  # X is the vector to fill down
  # Y is a boolean vector taking 1 where the spot should be filled from above
  
  CumSum   <-  cumsum(!Y)
  Match    <-  match(CumSum, CumSum)
  X[Match]
  
  return(X)
}


