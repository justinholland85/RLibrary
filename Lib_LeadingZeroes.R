Lib.Reverse.Repeat    <-  function(Times, What){
  
  return(rep(What, Times))
}

######################################################################################################


Lib.LeadingZeros   <-  function(X, d){
  
  X                  <-  as.character(X)
  Nchar              <-  nchar(X)
  
  if(missing(d)){d   <-  max(Nchar)}
  
  AddZeroes <- unlist(lapply(lapply(as.list(d - Nchar), Lib.Reverse.Repeat,"0"), paste, collapse=""))
  
  Y  <-  paste0(AddZeroes,   X)
  return(Y)
  
}


