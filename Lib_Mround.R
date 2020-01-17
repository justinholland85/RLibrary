
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


