
Lib.DummyDF     <-  function(ProForma, Input, N){
  
  if(missing(N)){N <-  length(Input[[1]])}
  if(missing(Input)){Input.Names   <-  character(0)} else {Input.Names <-  intersect(names(Input), names(ProForma))}
  
  Dummy     <-  list()
  N.Vars    <-  length(ProForma)
  Class     <-  unlist(lapply(ProForma, class))
  VarNames  <-  names(ProForma)
  
    for(i in 1:N.Vars){
    
    if(Class[i] == "numeric"){Dummy[[i]]    <-  as.numeric(rep(NA, N))}
    if(Class[i] == "integer"){Dummy[[i]]    <-  as.integer(rep(NA, N))}
    if(Class[i] == "character"){Dummy[[i]]  <-  as.character(rep(NA, N))}
    if(Class[i] == "logical"){Dummy[[i]]    <-  as.logical(rep(NA, N))}
    if(Class[i] == "factor"){Dummy[[i]]     <-  factor(rep(NA, N), levels = levels(ProForma[[i]]))}
  
    }

  names(Dummy)  <-  VarNames
  
  if(length(Input.Names) > 0){
    
    for(i in 1:length(Input.Names)){
      
      Dummy[[Input.Names[i]]]   <-  Input[[Input.Names[i]]]
      
    }
    
  } 
  
  return(data.frame(Dummy, stringsAsFactors = FALSE))
  
  
}
