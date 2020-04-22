
Lib.GA.1D.Dummy.Norm  <-  function(N,Mu,Sig){
  
  
  if(missing(N)){N    <- 10000}
  if(missing(Mu)){Mu  <- 0  }
  if(missing(Sig)){Sig  <- 10  }
  
  X  <-  rnorm(N,Mu,Sig)
  
  return(X)
  
}

######################################################################################################

Lib.GA.1D.Dummy.Norm.Pos  <-  function(N,Mu,Sig){
  
  
  if(missing(N)){N    <- 10000}
  if(missing(Mu)){Mu  <- 0  }
  if(missing(Sig)){Sig  <- 10  }
  
  X  <-  abs(rnorm(N,Mu,Sig))
  
  return(X)
  
}

######################################################################################################

Lib.GA.1D.Dummy.Norm.Neg  <-  function(N,Mu,Sig){
  
  
  if(missing(N)){N    <- 10000}
  if(missing(Mu)){Mu  <- 0  }
  if(missing(Sig)){Sig  <- 10  }
  
  X  <-  -abs(rnorm(N,Mu,Sig))
  
  return(X)
  
}

######################################################################################################

Lib.GA.1D.PowerLaw <-  function(N,Min,Alpha){
  
  
  if(missing(N)){N          <-  10000}
  if(missing(Min)){Min      <-  .1  }
  if(missing(Alpha)){Alpha  <-  4  }
  
  X  <- Lib.PowerLaw(Min = Min, Alpha = , n = N)
  X  <-  X * (-1) ^ (round(runif(N)))
  
  
  return(X)
  
}

######################################################################################################


Lib.GA.1D.PowerLaw.Pos <-  function(N,Min,Alpha){
  
  
  if(missing(N)){N          <-  10000}
  if(missing(Min)){Min       <-  .1  }
  if(missing(Alpha)){Alpha  <-  4  }
  
  X  <- Lib.PowerLaw(Min = Min, Alpha = , n = N)
  
  return(X)
  
}

######################################################################################################

Lib.GA.1D.PowerLaw.Neg <-  function(N,Min,Alpha){
  
  
  if(missing(N)){N          <-  10000}
  if(missing(Min)){Min       <-  .1  }
  if(missing(Alpha)){Alpha  <-  4  }
  
  X  <- -Lib.PowerLaw(Min = Min, Alpha = , n = N)
  
  return(X)
  
}

######################################################################################################

Lib.GA.1D.RandFunc   <-  function(N, K){
  
  if(missing(N)){N          <-  10000}
  
  Functions   <-  c("Lib.GA.1D.Dummy.Norm",
                    "Lib.GA.1D.Dummy.Norm.Pos",
                    "Lib.GA.1D.Dummy.Norm.Neg",
                    "Lib.GA.1D.PowerLaw",
                    "Lib.GA.1D.PowerLaw.Pos",
                    "Lib.GA.1D.PowerLaw.Neg")
  
  N.Funcs     <- length(Functions)  
  
  if(missing(K)){  K  <-  ceiling(runif(1,0, N.Funcs))}
  
  
  X  <-  do.call(Functions[K], args=list("N" = N))
  
  return(X)
  
}





