
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

Lib.Add.DummyVars  <-  function(DF, AddVars){
  
  N.Vars            <-   length(AddVars)
  N.Row             <-   nrow(DF)
  
  if(N.Vars > 0){
  
  Dummy.Add         <-  data.frame(rep(list(rep(NA, N.Row)), N.Vars))
  names(Dummy.Add)  <-  AddVars
  
  DF.Out            <-  data.frame(DF, Dummy.Add)
  
  }  else { DF.Out  <-  DF}
  
  return(DF.Out)
  
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
  
######################################################################################################

Lib.Append.DF    <-  function(DF1, DF2){
  
  
  DF1.a       <-  data.frame(lapply(DF1, Lib.FactorAsCharacter),       stringsAsFactors = FALSE)
  DF2.a       <-  data.frame(lapply(DF2, Lib.FactorAsCharacter),       stringsAsFactors = FALSE)
  
  AllNames    <-  c(names(DF1.a), setdiff(names(DF2.a), names(DF1.a)))
  
  AddVars.1   <-  setdiff(names(DF2), names(DF1))
  AddVars.2   <-  setdiff(names(DF1), names(DF2))
  
  DF1.a       <-  Lib.Add.DummyVars(DF1.a, AddVars.1)    
  DF2.a       <-  Lib.Add.DummyVars(DF2.a, AddVars.2)    
  
  DF          <-  Lib.ListBind.ByNames(list(DF1.a, DF2.a), AllNames)
  
  return(DF)
  
}


  
  
  
  