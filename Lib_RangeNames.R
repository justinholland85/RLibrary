Lib.RangeNames  <-  function(Breaks, Closed.Right){
  
  if(missing(Closed.Right)){Closed.Right   <-  FALSE}  
  
  N  <-  length(Breaks) - 1 
  
  Names <-  array(dim=N)
  
  for(i in 1:N ){
    
    if(i == N & Breaks[i+1] == Inf){
      
    Names[i]  <-  paste(c(Breaks[i]," <= X <= ", Breaks[i+1]),collapse = "")} else {
    
    if(Closed.Right == FALSE){  
    Names[i]  <-  paste(c(Breaks[i]," <= X < ", Breaks[i+1]),collapse = "")} else {
    Names[i]  <-  paste(c(Breaks[i]," < X <= ", Breaks[i+1]),collapse = "")}
      
    }
    
  }
  
  return(Names)
}