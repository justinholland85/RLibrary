Lib.Zeta  <-  function(X,r){
  
  if(missing(r)){r = 6}
  
  Primes <-  c(2,3,5,7,11,13,17,19,23,29,31,37,41,43)
  
  M  <-  dim(X)[1]
  N  <-  dim(X)[2]
  
  Multiplier  <-  t(array(dim=c(N,M),Primes[1:N]))^.5
  
  return(round(rowSums(X * Multiplier),r))
}
