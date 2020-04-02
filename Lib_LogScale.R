Lib.LogScale  <-  function(X,Base){
     
  if(missing(Base)){ Base <- 10}
  
     X[which( 0 < X & X < 1)]  <-  1
     X[which(-1 < X & X < 0)]  <-  -1
     
     Y  <-  sign(X) * log(1 + abs(X), Base)
     return(Y)
     
}

######################################################################################################



Lib.LogScale.Smart  <-  function(X,Base){
  
  if(missing(Base)){ Base <- 10}
  
  Min       <-  min(X)   
  Max       <-  max(X)
  
  Type.Pos    <-  Min >= 0 
  Type.Neg    <-  Max <= 0 
  Type.PosNeg <-  Min < 0 & Max > 0
  
  Type        <-  c("Pos", "Neg", "PosNeg")[which(c(Type.Pos, Type.Neg, Type.PosNeg))]
  
  
  if(Type %in% c("Pos", "Neg")){
    
    Y  <-  sign(X) * log(abs(X), Base)
    
  }
  
  if(Type == "PosNeg"){
  
  X[which( 0 < X & X < 1)]  <-  1
  X[which(-1 < X & X < 0)]  <-  -1
  
  Y  <-  sign(X) * log(1 + abs(X), Base)
  
  }
  
  return(Y)
  
}
