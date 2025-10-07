
Lib.FillDown  <-  function(X, Y){
  
  # X is the vector to fill down
  # Y is a boolean vector taking 1 where the spot should be filled from above
  
  X[Y == 1]  <-  X[cumsum(Y == 0)][Y == 1]
  
  return(X)
}