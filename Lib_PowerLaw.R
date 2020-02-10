Lib.PowerLaw   <-  function(u, Min, Alpha){
  
  return(Min * ((1 - u)^(1 / (1 - Alpha))))
  
}