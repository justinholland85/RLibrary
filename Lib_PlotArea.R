Lib.PlotArea   <-  function(X, Y.Lower, Y.Upper, Fill, BorderCol, BorderLwd, Eps ){
  
  if(missing(Fill)){Fill   <-  "white"}
  if(missing(BorderCol)){BorderCol   <-  "black"}
  if(missing(BorderLwd)){BorderLwd   <-1}
  if(missing(Eps)){Eps   <-  0}
  
  Delta  <-  Eps * (par()$usr[4] - par()$usr[3])
  
  Poly.X   <-  c(X, rev(X))
  Poly.Y   <-  c(Y.Lower, rev(Y.Upper - Delta))
  
  polygon(Poly.X, Poly.Y, col=Fill, border=BorderCol, lwd =  BorderLwd)
  
  
}

