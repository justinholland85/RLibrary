
Lib.HeatScale  <-  function(X, HeatScale, X.Min, X.Max){
  
  if(missing(X.Min)){ X.Min  <-  min(X) } 
  if(missing(X.Max)){ X.Max  <-  max(X) } 
  
  
  
  N.Colours    <-  length(HeatScale)
  
  Long.X       <-  c(X)
  

  
  
  Del          <-   (X.Max - X.Min) / (N.Colours -1 )
  
  Int          <-   pmax(pmin(ceiling(1 + (Long.X - X.Min) / Del ), N.Colours) , 1) 
  
  Colour       <-  X
  
  Colour[]     <-  HeatScale[Int]
   
  Output       <-  list("Colour" = Colour)
  
  return(Output)
  
  
}




