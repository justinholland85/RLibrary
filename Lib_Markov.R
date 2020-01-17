

Lib.ArbRand  <-  function(P, R, N){
     
     if(missing(N) & is.vector(P)){N <- 1}
     if(missing(N) & is.matrix(P)){N <- nrow(P)}
     if(missing(R)){R  <-  runif(N)}
     
     P   <-  P /  rowSums(P)
     M   <-  ncol(P) 
     PP  <-  cbind(0, Lib.Shell(P, 0))
     X   <-  rep(0, N)
     
     
     for(i in 1:M){ PP[,i+1] <-  PP[,i] + P[,i]   }
     for(i in 1:M){ X <- X + (R > PP[,i])}
     
     return(X)
     
}




Lib.Markov  <-  function(X, P, N, M){

     if(!missing(P)){ N <- nrow(P)}
     if(!missing(P) & missing(X)){X <- rep(100, N)}
     if(!missing(X) & missing(P)){N <- length(X)}
     
     if(missing(N) & missing(P) & missing(X)){N  <- 5}
     if(missing(X)){X <- rep(100, N)}
     
     if(missing(P)){P <- array(dim = c(N, N), 1 / N )}
     if(missing(M)){M <- 1}
     
     Pars   <-  list("X" = X,
                     "P" = P,
                     "N" = N)
     
     Z    <-  rep(seq(1,N), X)  
     Z.N  <-  length(Z)
     Z    <-  list(Z)
     
     R    <-  list()
     V    <-  list()
     P    <-  Lib.RowsToList(P)
     
     
     for(i in 1:M){R[[i]]  <-  runif(Z.N) }      
     
     for(i in 1:M){
     i.Z.P     <-  do.call(rbind, P[Z[[i]]]) 
     Z[[i+1]]  <-  Lib.ArbRand(i.Z.P, R =  R[[i]])     
     }
     
     for(i in 1:M){
     V[[i]]  <-  table(Z[[i]], Z[[i + 1]])
     }

     X   <-  lapply(Z, table)
     X   <-  Lib.MapListToSet(X)
     X   <-  do.call(cbind, X )
     
     colnames(X)  <-  seq(0, M)
     
     if(M == 1){V <- V[[1]]}
     
     Output  <-  list("V" = V, 
                      "X" = X,
                      "Pars" = Pars)
     
     return(Output)
     
}




