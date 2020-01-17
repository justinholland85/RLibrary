Lib.LogScale  <-  function(X,Base){
     
  if(missing(Base)){ Base <- 10}
  
     X[which( 0 < X & X < 1)]  <-  1
     X[which(-1 < X & X < 0)]  <-  -1
     
     Y  <-  sign(X) * log(1 + abs(X), Base)
     return(Y)
     
}

