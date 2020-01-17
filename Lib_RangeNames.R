Lib.RangeNames  <-  function(Breaks){
  
  
  N  <-  length(Breaks) - 1 
  
  Names <-  array(dim=N)
  
  for(i in 1:N ){
    
    if(i == N & Breaks[i+1] == Inf){
      
    Names[i]  <-  paste(c(Breaks[i]," <= X <= ", Breaks[i+1]),collapse = "")} else {
    
    Names[i]  <-  paste(c(Breaks[i]," <= X < ", Breaks[i+1]),collapse = "")}
    
  }
  
  return(Names)
}