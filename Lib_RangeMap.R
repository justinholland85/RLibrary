# Lib.RangeMap  <-  function(X,R){
  
#  Y  <-  rep(0, length(X))
#  N  <-  length(R) - 1 
  
#  for(i in 1:N){
    
#    if(R[i+1] == Inf){
    
#    Y  <-  Y + i * (R[i] <= X & X <= R[i+1])
      
#    } else {
    
#    Y  <-  Y + i * (R[i] <= X & X < R[i+1])
    
#    }
    
#  }
  
#  return(Y)
  
#}

######################################################################################################

Lib.RangeMap  <-  function(X,R, Closed.Right){ 

if(missing(Closed.Right)){Closed.Right   <-  FALSE}  
  
Cut     <-  cut(X, breaks = R, right = Closed.Right, include.lowest = TRUE )
Levels  <-  levels(Cut)

Value   <-  Lib.NA.To.Zero(as.numeric(Cut))
Value[which(is.na(X))]  <-  NA

return(Value)


}













