Lib.InvertedFill  <-  function(ProForma, X, Dim){
  
  if(!missing(ProForma)){Dim <- dim(ProForma)}
  
  return(t(array(dim=rev(Dim), X)))
  
}
