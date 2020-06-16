Lib.RangeMap  <-  function(X,R){
  
  Y  <-  rep(0, length(X))
  N  <-  length(R) - 1 
  
  for(i in 1:N){
    
    if(R[i+1] == Inf){
    
    Y  <-  Y + i * (R[i] <= X & X <= R[i+1])
      
    } else {
    
    Y  <-  Y + i * (R[i] <= X & X < R[i+1])
    
    }
    
  }
  
  return(Y)
  
}

######################################################################################################

Lib.RangeMap.Cut  <-  function(X,R){ 

Cut     <-  cut(X, breaks = R, right = FALSE, include.lowest = TRUE )
Levels  <-  levels(Cut)

Value   <-  Lib.NA.To.Zero(as.numeric(Cut))

return(Value)


}


