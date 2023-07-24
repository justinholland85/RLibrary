


Lib.PrimesCalc   <-  function(N = 100){
  
  # N is the upper bound, i.e. find all primes less than N
  
  Seq    <- seq(1,N)
  
  Primes <-  c(2)
  
  Test   <-  rep(0, N)
  
  repeat{
  Test  <- Test + Seq %in% (max(Primes) * Seq)  
  
  
  p  <- which(Test == 0)[2]
  
  if(is.na(p)){break} else {Primes[[length(Primes) + 1]] <- p}
  
  }
  
  
  return(Primes)
  
}