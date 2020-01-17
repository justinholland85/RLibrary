
Lib.FactorAsCharacter <-  function(X){
  
  if(class(X) == "factor"){X  <-   as.character(X)}
  return(X)
}

######################################################################################################

Lib.Rbind.DF  <-  function(DF1, DF2){
  
  # This will hopefully be faster than rbind() on two data frames
  # Oviously doesnt work for factors but could probably be made to.
  
  DF1.a   <-  data.frame(lapply(DF1, Lib.FactorAsCharacter),       stringsAsFactors = FALSE)
  DF2.a   <-  data.frame(lapply(DF2, Lib.FactorAsCharacter),       stringsAsFactors = FALSE)
  

  DF      <-  data.frame(mapply(c, DF1.a, DF2.a, SIMPLIFY = FALSE), stringsAsFactors = FALSE)
  
  return(DF)
  
}

######################################################################################################


Lib.ListBind.ByNames  <-  function(List, Names){
  
  if(missing(Names)){ Names   <-  Reduce(intersect, lapply(List, names)) }
  
  Joined    <-  list()
  
  N.Vars   <-  length(Names)
  
  for(i in 1:N.Vars){
    
    Joined[[i]]   <-  unlist(lapply(List, Lib.Take.One, Names[i]))
    
  }
  
  
  names(Joined)  <-  Names
  
  DF   <-  as.data.frame(Joined, stringsAsFactors = FALSE)
  
  dim(DF)
  
  return(DF)
  
}
  
  
  
  
  
  