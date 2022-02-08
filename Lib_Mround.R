
Lib.Mround  <-  function(X,M,Dir){
  # Dir 0 will round to nearest multiple
  # Dir -1 will round down to nearest multile (absolute terms)
  # Dir 1 will round up to nearest multiple (absolute terms)
  # Positives and negatives are symmetric 
  
  if(missing(Dir)){Dir  <-  0}
  
  
  S  <-  sign(X)
  
  if(Dir ==  0){D  <-  round(abs(X/M)) }
  if(Dir == -1){D  <-  floor(abs(X/M)) }
  if(Dir ==  1){D  <-  ceiling(abs(X/M)) }
  
  Y  <-  S * D * M
  
  return(Y)
  
  
}


######################################################################################################

Lib.PrintNumber   <-  function(X){
  
  
  
  K1    <-  ceiling(log(X, 1000 ))
  
  
  Parts    <-  list()
  
  for(i in 1:K1){
    
    Parts[[i]]   <-  floor(X  / (1000 ^ (i - 1))) %% 1000
    
  }
  
  
  
  Parts       <-  rev(Parts)
  P1          <-  Parts[[1]]
  
  Parts       <-  lapply(Parts, Lib.LeadingZeros, 3)
  Parts[[1]]  <-  P1
  
  
  Y  <-  paste0(Parts, collapse = ",")
  
  return(Y)
  
}