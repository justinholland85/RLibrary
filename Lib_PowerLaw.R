Lib.PowerLaw   <-  function(u, Min, Alpha, n){
  
  if(missing(n))    {n     <- 1}
  if(missing(Min))  {Min   <-  1}
  if(missing(Alpha)){Alpha <-  2}
  if(missing(u))    {u     <-  runif(n)}
  
  return(Min * ((1 - u)^(1 / (1 - Alpha))))
  
}