Lib.VecToList  <-  function(X){

N  <-  length(X)
Seq  <-  seq(1,N)
List  <-  tapply(X, Seq, identity, simplify=FALSE)
return(List)
}

