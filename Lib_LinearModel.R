Lib.Linear.Model   <-  function(Alpha, Beta, F.X, Pars.X, F.Eps, Pars.Eps){
     
     X         <-  cbind(as.matrix(mapply(do.call, F.X, Pars.X)))
     
     N         <-  dim(X)[1]
     
     Beta.Mat  <-  t(array(dim=c(length(Beta), N), Beta))
 
     Eps       <-  do.call(F.Eps,  Pars.Eps)     
         
     Y         <- Alpha +  rowSums(Beta.Mat * X) + Eps
     

     Output    <-  list("X" = X,
                        "Y" = Y,
                        "Eps" = Eps)
     
     return(Output)
     
}


