
Lib.LeftJoin  <-  function(X, Y, Match, Vars){
  
  
  if(missing(Vars)){Vars   <-  setdiff(names(Y), names(X))}
  
  
  for(i in 1:length(Vars)){
  
  i.Var   <-  Vars[i]
    
  Text  <-  paste0("X$",i.Var, "  <-  Y$", i.Var, "[Match]")
  
  eval(parse(text = Text))
  }
  
  return(X)
  
}