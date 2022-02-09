
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
  
  # Note: not vectorised
  
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

######################################################################################################

Lib.PrintNumber   <-  function(X){
  
  # Note: not vectorised
  
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

Lib.PrintNumber(3000123)



X    <-  123456


Lib.PrintSignif   <-  function(X, d){
  
  X   <-  signif(X, d)
  
  Mag    <-  floor(log(X, 10))
  
  D      <-  numeric(0)
  M      <-  numeric(0)
  
  for(i in 1:d){
    
    M[[i]]    <-  Mag - (i-1)
    
    # some kind of floating point issue, the 12 may need to be fixed for small numbers
    D[[i]]    <- floor( round(X %% (10 ^  (M[[i]] + 1)) / 10 ^ M[[i]], 12))
    
  }
  
  D[which(D == 10)]  <-  0
  
  Y  <-  as.character(D)
  
  DP    <-    match(0, M) 
  
  if(!is.na(DP) & DP  < d ){
    
    P0   <-  Y[1:DP]
    P1   <-  Y[(DP+1):d]
    
    Y   <-  c(P0, ".", P1)
    
  }  
  
  
  
  if(M[1] < 0) {
    
    LZ     <-  c("0.", rep("0", abs(M[1]) - 1))
    
  } else { LZ  <-  character(0)}
  
  
  if(M[d] > 0 ){ 
    TZ   <-  rep("0", M[d])
    
  } else {TZ <-  character(0)}
  
  Z   <-  c(LZ, Y, TZ)
  
  Z   <-  paste0(Z, collapse = "")  
  
  
  return(Z)
  
  
}







